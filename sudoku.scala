//Names: Betsy Weber & Phil Goldberg
//Emails: elizabeth.a.weber@vanderbilt.edu,
//        phillip.a.goldberg@vanderbilt.edu
//VUnetIDs: weberea2 & goldbepa
//Class: CS3270
//Date: 11/29/16
//I have neither given nor received help on this assignment
//Description: This program is a sudoku solver written in scala
//for project 4. It takes in a text file with the puzzle and 
//the solution (or a message if the puzzle is unsolvable)
//to the user

import scala.io.Source

object Puzzle extends App{

  //define the dimensions of the board
  val MAX_ROW=9
  val MAX_COL=9

  //create the sudoku board
  var Grid=Array.ofDim[Int](MAX_ROW,MAX_COL)

  //read the sudoku puzzle values in from the text file
  val filename = readLine("Sudoku puzzle file name: ")
  val linesFromText=Source.fromFile(filename).getLines().toArray
  var row=0
  for(lines<-linesFromText){
    val arr=lines.split(" ").map(x=>x.toInt)
    for(i<- 0 until MAX_COL){
      Grid(row)(i)=arr(i)
    }
    row+=1
  }
  
  print("Board entered:\n")
  printBoard()

  //call the recursive backtracking solver
  var isSolved = solver(0,0)
  //print the results of the solver method, depending
  //on whether or not the puzzle was solved
  if(isSolved){
    print("Solution:\n")
    printBoard()
  } else{
    print("No solution to this puzzle\n")
  }

  //start of helper method definitions

  /**
    *
    * @param row
    * @param num
    * @return boolean, if the Row is clear without the number, 
    * it'll return true, otherwise false
    */
  private def checkRow(row:Int,num:Int):Boolean={
    //check if num is in input row
    for(x<- 0 until 9)
    {
      if(Grid(row)(x)==num) return false
    }
    return true
  }

  /**
    *
    * @param col
    * @param num
    * @return boolean, if the Column is clear without the number, 
    * return true, otherwise false
    */
  private def checkCol(col:Int,num:Int):Boolean={
    //check if num is in input column
    for(x<- 0 until 9)
    {
      if(Grid(x)(col)==num) return false
    }
    return true
  }
  
  /**
   *
   * @param row
   * @param col
   * @param num
   * @return boolean, if the square is clear without the number,
   * return true, otherwise false 
   */
  private def checkSect(row:Int,col:Int,num:Int):Boolean ={
    //determine which Box the input cell is in
    val sectCol:Int = col/3
    val sectRow:Int = row/3

    val cStart:Int = sectCol*3
    val rStart:Int = sectRow*3
    
    //check if num is already in the Box
    for(rrow<- rStart until rStart+3)
       for(ccol<- cStart until cStart+3)
    {
       if(Grid(rrow)(ccol)==num) return false
    }
    //num was not in box, so return true
    return true
  }

  /**
   * 
   * @param row
   * @param col
   * @param num
   * @return boolean, if it is safe to place the number in
   * that cell (considering the column,row, and box it is in)
   * then return true, otherwise false
   */
  private def isValid(row:Int,col:Int,num:Int):Boolean={
    return checkCol(col,num)&&checkRow(row,num)&&checkSect(row,col,num);
  }

  /**
   * 
   * @param row
   * @param col
   * @return boolean, return true if puzzle was solved, otherwise
   * return false
   */
  private def solver(row:Int,col:Int):Boolean={
    //check if end of puzzle has been reached
    if(row==9) return true

    //check if end of row has been reached
    if(col==9){
      return solver(row+1,0)
    }
    
    //check if this cell already has an assigned number
    //and call solver on next cell if it does
    if(Grid(row)(col)!=0)
    {
      //if(solver(row,col+1)) return true
      return solver(row, col+1)
    }

    //try assigning a number to the cell and check if 
    //that number is valid
    for(i<-1 until 10)
    {
      if(isValid(row,col,i)) {
        Grid(row)(col) = i
        if(solver(row,col+1)) return true
        Grid(row)(col)=0
      }
    }
    
    //no value was safe for this cell, so backtrack
    Grid(row)(col) = 0
    return false
  }
  
  /**
   * this method prints the sudoku puzzle to the user
   */
  private def printBoard() {
    var i =5
    for(row<- 0 until 9){
      for(col<- 0 until 9){
        print(Grid(row)(col)+" ")
        if(col == 2 || col == 5){
          print("| ")
        }
      }
      print("\n")
      if(row == 2 || row == 5){
        print("------+-------+------\n")
      }
    }
  }

}

