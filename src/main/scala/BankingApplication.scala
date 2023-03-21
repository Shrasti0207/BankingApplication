import scala.util.Random
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

object BankingApplication {
  val allAccount: Map[Long,Double]=Map()
  val acNumberList: ListBuffer[Long] = ListBuffer()
}
case class Transactions(transactionId: Long, accountNumber: Long, transactionType: String, amount: Double)
class AccountMethods{
  def createAccount(openingBalance: Double): Map[Long, Double] = {
    val generateAccountNumber = Random.nextLong.abs
    BankingApplication.acNumberList.addOne(generateAccountNumber)
    BankingApplication.allAccount += (generateAccountNumber -> openingBalance)
    BankingApplication.allAccount
  }
  def listAllAccounts(): Map[Long,Double]={
    BankingApplication.allAccount
  }
  def fetchAccountBalance(accountNumber: Long): Double ={
    BankingApplication.allAccount.getOrElse(accountNumber, -1)
  }
  def updateBalance(transactions: List[Transactions]): Map[Long, Double]= {
    transactions.foreach(transaction => {
      val currentBalance = fetchAccountBalance(transaction.accountNumber)
      transaction.transactionType match {
        case "credit" => BankingApplication.allAccount += (transaction.accountNumber -> (currentBalance + transaction.amount))
        case "debit" => BankingApplication.allAccount += (transaction.accountNumber -> (currentBalance - transaction.amount))
        case _ => throw new IllegalArgumentException()
      }
    })
    BankingApplication.allAccount
  }
  def deleteAccount(accountNumber: Long): Boolean = {
    if(BankingApplication.allAccount.contains(accountNumber)){
      BankingApplication.allAccount.remove(accountNumber)
      true
    }
    else{
      false
    }
  }
}
