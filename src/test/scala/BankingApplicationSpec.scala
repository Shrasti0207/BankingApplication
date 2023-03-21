import org.scalatest.funsuite.AnyFunSuite
class BankingApplicationSpec extends AnyFunSuite {
  val accountMethods = new AccountMethods()
  val account1 = accountMethods.createAccount(4000.0)
  val account2 = accountMethods.createAccount(1000.0)
  val account3 = accountMethods.createAccount(500.0)

  test(" Test for create Account") {
    assert(BankingApplication.allAccount.size==3)
  }
  test("Test for list all accounts") {
    assert(accountMethods.listAllAccounts() == BankingApplication.allAccount)
  }
  test("Test for Fetch Accounts"){
    assert(accountMethods.fetchAccountBalance(BankingApplication.acNumberList(1)) == 1000.00)
  }

  test("Update account balance for credit transaction") {
    val transactions = List(Transactions(1, BankingApplication.acNumberList.head, "credit", 500.00))
    accountMethods.updateBalance(transactions)
    assert(accountMethods.fetchAccountBalance(BankingApplication.acNumberList.head) == BankingApplication.allAccount.getOrElse(BankingApplication.acNumberList.head,0))
  }

  test("Update account balance for debit transaction") {
    val transactions = List(Transactions(1, BankingApplication.acNumberList.head, "debit", 500.00))
    accountMethods.updateBalance(transactions)
    assert(accountMethods.fetchAccountBalance(BankingApplication.acNumberList.head) == BankingApplication.allAccount.getOrElse(BankingApplication.acNumberList.head, 0))
  }

  test("Test for Delete Accounts"){
    assert(accountMethods.deleteAccount(BankingApplication.acNumberList(2)))
  }
}
