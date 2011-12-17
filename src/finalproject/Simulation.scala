package finalproject

import io.Source
import collection.mutable
import actors.Actor._
import org.scalaquery._
import org.scalaquery.ql.extended.{ExtendedTable=>Table}
import org.scalaquery.ql.extended.SQLiteDriver.Implicit._
import org.scalaquery.session.Database
import org.scalaquery.session.Database.threadLocalSession

/**
 * Created by IntelliJ IDEA.
 * User: Matt
 * Date: 12/13/11
 * Time: 2:53 PM
 * To change this template use File | Settings | File Templates.
 */

object Simulation
{
  val nodes = mutable.Map.empty[String, List[(String, Double)]]
  //var vehicles = List[vehicle]
  val Vehicles = new Table[(Int,String,Int,Int)]("VEHICLES")
  {
    def id = column[Int]("VEH_ID",O.PrimaryKey)
    def name = column[String]("VEH_NAME")
    def nodes = column[Int]("VEH_NODES")
    def time = column[Int]("VEH_TIME")
    def * = id ~ name ~ nodes ~ time
  }
  val Nodes = new Table[(String)]("NODEs") //records number of nodes
  {
    def node = column[String]("NODE_NAME",O.PrimaryKey)
    def * = node
  }
  val db = Database.forURL("jdbc:sqlite::memory:",driver="org.sqte.JDBC")
  db withSession
  {
    (Vehicles.ddl ++ Nodes.ddl).create
  }

  def main(args: Array[String])
  {
    setup(args); //read input files
    /*for(v <- vehicles)
    {
      v.start;
    }*/
    loop
    {
      react
      {
        case x => x; //placeholder for vehicle responses
      }
    }
  }

  def setup(args: Array[String])
  {
    if(args.length > 1)
    {
      for(line <- Source.fromFile(args(0)).getLines())
      {
        //line format is name|name,length|name,length|...
        val strline = line.mkString.split("|")
        for(i <- 1 to strline.length)
        {
          nodes(strline(0)) = (strline(i).split(",")(0),strline(i).split(",")(1).toDouble)::nodes(strline(0))
          db withSession
          {
            Nodes.insert(strline(0))
          }
        }
      }
      var i: Int = 0
      for(line <- Source.fromFile(args(1)).getLines())
      {
        //construct new vehicle for each line in a list
        //pass schedule to each vehicle's list in constructor
        //line format is name|node,node,node...
        val strline = line.mkString.split("|")
        //vehicles = new vehicle(strline(0),strline(1).split(","),self)::vehicles //placeholder for vehicles
        db withSession
        {
          Vehicles.insert(i,strline(0),strline(1).split(",").length,-1)
        }
        i+= 1
      }
    }
    else
    {
      Console.err.println("Incomplete simulation setup, please try again")
    }
  }
  def dbsetup()
  {
    val db = Database.forURL("jdbc:sqlite::memory:",driver = "org.sqlite.JDBC")
  }
}