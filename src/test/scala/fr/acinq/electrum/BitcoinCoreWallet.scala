/*
 * Copyright 2018 ACINQ SAS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fr.acinq.electrum

import fr.acinq.bitcoin._
import grizzled.slf4j.Logging
import org.json4s.JsonAST._

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by PM on 06/07/2017.
  */
class BitcoinCoreWallet(rpcClient: BitcoinJsonRPCClient)(implicit ec: ExecutionContext) extends Logging {

  import BitcoinCoreWallet._

  def fundTransaction(hex: String, lockUnspents: Boolean, feeRatePerKw: Long): Future[FundTransactionResponse] = {
    val feeRatePerKB = BigDecimal(feerateKw2KB(feeRatePerKw))
    rpcClient.invoke("fundrawtransaction", hex, Options(lockUnspents, feeRatePerKB.bigDecimal.scaleByPowerOfTen(-8))).map(json => {
      val JString(hex) = json \ "hex"
      val JInt(changepos) = json \ "changepos"
      val JDecimal(fee) = json \ "fee"
      FundTransactionResponse(Transaction.read(hex), changepos.intValue(), Satoshi(fee.bigDecimal.scaleByPowerOfTen(8).longValue()))
    })
  }

  def fundTransaction(tx: Transaction, lockUnspents: Boolean, feeRatePerKw: Long): Future[FundTransactionResponse] = fundTransaction(Transaction.write(tx).toString(), lockUnspents, feeRatePerKw)

  def signTransaction(hex: String): Future[SignTransactionResponse] =
    rpcClient.invoke("signrawtransaction", hex).map(json => {
      val JString(hex) = json \ "hex"
      val JBool(complete) = json \ "complete"
      SignTransactionResponse(Transaction.read(hex), complete)
    })

  def signTransaction(tx: Transaction): Future[SignTransactionResponse] = signTransaction(Transaction.write(tx).toString())

  def getTransaction(txid: BinaryData): Future[Transaction] = rpcClient.invoke("getrawtransaction", txid.toString()) collect { case JString(hex) => Transaction.read(hex) }

  def publishTransaction(tx: Transaction)(implicit ec: ExecutionContext): Future[String] = publishTransaction(Transaction.write(tx).toString())

  def publishTransaction(hex: String)(implicit ec: ExecutionContext): Future[String] = rpcClient.invoke("sendrawtransaction", hex) collect { case JString(txid) => txid }

  def unlockOutpoints(outPoints: Seq[OutPoint])(implicit ec: ExecutionContext): Future[Boolean] = rpcClient.invoke("lockunspent", true, outPoints.toList.map(outPoint => Utxo(outPoint.txid.toString, outPoint.index))) collect { case JBool(result) => result }


  def getBalance: Future[Satoshi] = rpcClient.invoke("getbalance") collect { case JDecimal(balance) => Satoshi(balance.bigDecimal.scaleByPowerOfTen(8).longValue()) }

  def getFinalAddress: Future[String] = for {
    JString(address) <- rpcClient.invoke("getnewaddress")
  } yield address

  private def signTransactionOrUnlock(tx: Transaction): Future[SignTransactionResponse] = {
    val f = signTransaction(tx)
    // if signature fails (e.g. because wallet is uncrypted) we need to unlock the utxos
    f.recoverWith { case _ =>
      unlockOutpoints(tx.txIn.map(_.outPoint))
        .recover { case t: Throwable => logger.warn(s"Cannot unlock failed transaction's UTXOs txid=${tx.txid}", t); t } // no-op, just add a log in case of failure
        .flatMap { case _ => f } // return signTransaction error
        .recoverWith { case _ => f } // return signTransaction error
    }
  }
}

object BitcoinCoreWallet {

  // @formatter:off
  case class Options(lockUnspents: Boolean, feeRate: BigDecimal)
  case class Utxo(txid: String, vout: Long)
  case class FundTransactionResponse(tx: Transaction, changepos: Int, fee: Satoshi)
  case class SignTransactionResponse(tx: Transaction, complete: Boolean)
  // @formatter:on

  val MinimumFeeratePerKw = 253

  /**
    * Converts feerate in satoshi-per-kilobytes to feerate in satoshi-per-kw
    *
    * @param feeratePerKB fee rate in satoshi-per-kilobytes
    * @return feerate in satoshi-per-kw
    */
  def feerateKB2Kw(feeratePerKB: Long): Long = Math.max(feeratePerKB / 4, MinimumFeeratePerKw)

  /**
    *
    * @param feeratesPerKw fee rate in satoshi-per-kw
    * @return fee rate in satoshi-per-kilobyte
    */
  def feerateKw2KB(feeratesPerKw: Long): Long = feeratesPerKw * 4

}