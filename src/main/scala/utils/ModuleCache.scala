package utils

import chisel3._
import chisel3.experimental.hierarchy.{Definition, Instance}
import scala.collection.mutable
import scala.reflect.ClassTag

object ModuleCache {
  private case class EntryKey(klass: Class[_], implKey: Any)

  private val cache = mutable.HashMap[EntryKey, Definition[_]]()

  def apply[T <: Module : ClassTag](con: => T, implKey: Any = ()): Instance[T] = {
    val entry = EntryKey(implicitly[ClassTag[T]].getClass(), implKey)
    val defn = cache.get(entry) match {
      case Some(value) =>
        value.asInstanceOf[Definition[T]]
      case _ =>
        val defnNew = Definition(con)
        cache += (entry -> defnNew)
        defnNew
    }
    Instance(defn)
  }
}
