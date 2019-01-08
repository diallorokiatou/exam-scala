package com.fr.upem.partiel

import javax.sql.rowset.spi.TransactionalWriter

// Part2 (10pts)
/**
  *
  * The goal is to create a system that allows users to handle their personal finances.
  * Each user has an account with his UNIQUELY IDENTIFIED transactions.
  * Each transaction can be categorized with categories such as: Salary, Purchase, Withdrawal, Checks (deposits and payments) etc.
  *
  */
object Part2 {

  // 2.1 Modelling.
  // Create a model for the user's bank account
  case class User(id : Int) {
    def getId = id
  }

  // Create a model for a transaction (has an amount and a date)
    case class Date(jour:Int,mois:Int,annee:Int) {
    override def toString = s"$jour/$mois/$annee"
  }

  sealed trait  Categorize

   sealed trait Transaction extends Categorize {
    def getAmount : Double
    def getDate : Date
     def getType : String
  }

  /*case class Transaction(amount:Double, date :Date) extends Transaction {
    override def getAmount: Double = ???
    override def getDate: Date = ???
    def getType : String = ???
  }*/

  // Create a model for the following categories [salary, purchase, check deposit, check payment, withdrawal]
    case class Salary(amount:Double, date :Date) extends Transaction {
    override def getAmount: Double = amount
    override def getDate : Date = date
    def getType : String = "Salary"
  }

  case class Purchase(amount:Double, date :Date) extends Transaction{
    override def getAmount: Double = amount
    override def getDate : Date = date
    def getType : String = "Purshase"
  }

  trait Check extends Transaction {
    override def getAmount: Double
    override def getDate : Date
    def getType : String = "Check"
  }

  case class CheckPayement(amount:Double, date :Date) extends Check {
    override def getAmount: Double = amount
    override def getDate: Date = date
    override  def getType : String = super.getType + "-payement"
  }

  case class CheckDeposit(amount:Double, date :Date) extends Check {
    override def getAmount: Double = amount
    override def getDate: Date = date
    override  def getType : String = super.getType + "-deposit"
  }

  // 2.2 Create api
  // Create an api that allows for:
  // - Adding a transaction to a bank account
  // - Adding a transaction to a bank account with it's category
  // - Categorizing or recategorizing an existing transaction
  //
  // help: The bank account must save, for each transaction id, the transaction and it's eventual category
  // This could be achieved through a structure of this kind:
  // BankAccount(transactions: Map[TransactionId, CategorizedTransaction])
   object Main extends App {


    sealed
    case class BankAccount(transactions: Map[Int, Transaction]) {
       def addTransaction(id : Int,amount: Double, date: Date) = Purchase(amount,date)::transactions.toList

      def addTransaction(id: Int,amount:Double,date:Date,categorize:String) = categorize match {
        case "Salary" => Salary(amount,date)::transactions.toList
        case "Purchase" => Purchase(amount,date)::transactions.toList
        case "checkPayeent" => CheckPayement(amount,date)::transactions.toList
        case "checkDeposit" => CheckDeposit(amount,date)::transactions.toList
      }

      def categorize(idTransaction:Int, categorize:String) =  categorize match {
        case "Salary" => transactions.get(idTransaction).map(x =>Salary(x.getAmount,x.getDate) )
        case "Purchase" => transactions.get(idTransaction).map(x =>Purchase(x.getAmount,x.getDate) )
        case "checkPayement" => transactions.get(idTransaction).map(x =>CheckPayement(x.getAmount,x.getDate) )
        case "checkDeposit" => transactions.get(idTransaction).map(x =>CheckDeposit(x.getAmount,x.getDate) )
      }
    }

    // 2.3 Use the api that you just created.
    // - Create an empty account
    val bankAccount = BankAccount(Map.empty)
    // - Add a transaction with id 1 of amount -13 (and any date)
    bankAccount.addTransaction(-13,Date(12,12,2002))
    // - Add a transaction with id 2 of amount -50 (and any date)
    // - Add a check payment with id 3 of amount 650 (and any date)
    // - Categorize the second transaction (id "2") as a withdrawal
    // - (Re)categorize the third transaction (id "3") as check deposit
    //
    // help: After the above operations the bank account should hold:
    // TransactionId(1) -> (Transaction(1, -13, date), None)
    // TransactionId(2) -> (Transaction(2, -50, date), Some(Withdrawal))
    // TransactionId(3) -> (Transaction(3, 650, date), Some(CheckDeposit)

    // 2.4 CSV Export
    // Users want to be able to export their accounts in CSV (Comma-Separated Values) format.
    // A line is structured as follows: Id, Type, Amount, Date
    // Allow exporting a bank account as a CSV (no need to write a file, just write a String).
    // Amounts do not need to be formatted, write dates in any valid format (timestamp, ISO-8601 ...)
    /* def exportAsCSV(bankAccount: BankAccount) : String = bankAccount.transactions.toList.zipWithIndex match {
      case Nil => ""
      case (i,x) => s"$i,$x.."
  }*/

    // Example output:
    // 1,check-deposit,300,1546784990415
    // 2,purchase,-24,1546698590604
    // 3,salary,3500,1546612190770
    // 4,,24,1546612190770

    // 2.5 CSV Import
    // Users want to be able to import transactions from a CSV.
    // Write code to parse and validate csv files
    // Validation: The input data should be validated
    //
    // Example valid input
    // 1,check-deposit,300,1546784990415
    // 2,purchase,-24,1546698590604
    // 3,salary,3500,1546612190770
    // 4,,24,1546612190770
    //
    // Example invalid input
    // 1,invalid type,invalid amount,invalid date

    // 2.6 Extend the api data analysis features
    // It should allow for:
    // - Sum all incomes (salaries, check deposits, uncategorized positive transactions)
    // - List all check (deposit and payment) operations
    // - Compute the account balance

  }
}
