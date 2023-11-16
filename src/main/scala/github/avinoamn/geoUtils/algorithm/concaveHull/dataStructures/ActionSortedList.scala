package github.avinoamn.geoUtils.algorithm.concaveHull.dataStructures

import scala.annotation.tailrec
import Ordering.Implicits._

/** A generic action sorted List of T's.
 *
 * "Action sorted" means that for every item that we pass during the sorting process,
 * a user provided action will be triggered against this item.
 *
 * @param head The first item of the sorted list.
 * @tparam T The sorted list's items type.
 */
class ActionSortedList[T](head: T)(implicit ordering: Ordering[T]) {

  /** The list that contains the sorted items. */
  var list: List[T] = List(head)

  /** Index of the latest item inserted or passed during the sort process. */
  var currentIndex: Int = 0

  /** Sorted lists of items that are greater/lesser from the current index's item,
   * and pending to be inserted and actioned against on the next `addGreater`/`addLesser` call. */
  var greaterPendingItems: List[T] = List()
  var lesserPendingItems: List[T] = List()

  /** Gets the current index's item. */
  def getCurrentIndexItem: T = {
    list(currentIndex)
  }

  /** Add a list of sorted items to be pending. */
  def addPendingItems(items: List[T]): Unit = {
    if (items.head < getCurrentIndexItem) {
      addLesserPendingItems(items)
    } else {
      addGreaterPendingItems(items)
    }
  }

  /** Prepend items to `greaterPendingItems`. */
  private def addGreaterPendingItems(items: List[T]): Unit = {
    greaterPendingItems = items ++ greaterPendingItems
  }

  /** Prepend items to `lesserPendingItems`. */
  private def addLesserPendingItems(items: List[T]): Unit = {
    lesserPendingItems = items ++ lesserPendingItems
  }

  /** Handles the greater pending items list.
   *
   * Checks if there's a greater pending item that can be sorted in.
   * If exists, adds the item to the sorted list at the current index,
   * and the next sort action will be executed against it.
   *
   * @param item Item to be inserted on the `addGreater` method.
   * @param currentIndexItem Current index's item.
   */
  private def handleGreaterPendingItems(item: T, currentIndexItem: T): Unit = {
    if (greaterPendingItems.nonEmpty) {
      val comparablePendingItem = greaterPendingItems.head

      if (comparablePendingItem <= item && comparablePendingItem <= currentIndexItem) {
        list = list.patch(currentIndex, List(greaterPendingItems.head), 0);

        greaterPendingItems = greaterPendingItems.tail
      }
    }
  }

  /** Add to the sorted list an item that's greater than the current index's item,
   * while performing an action against every item that passes by during the sort process.
   *
   * @param item Item to be inserted.
   * @param sortAction Action to apply against every item that passes by during the sort process.
   */
  def addGreater(item: T, sortAction: T => Unit): Unit = {

    /** Recursive sort method that keeps looping as long as there are items that are
     * lesser/equal to the item that we are sorting in. */
    @tailrec
    def recursiveSort(): Unit = {
      if (currentIndex < list.length) {
        handleGreaterPendingItems(item, getCurrentIndexItem)

        if (item >= getCurrentIndexItem) {
          sortAction(getCurrentIndexItem)

          currentIndex += 1
          recursiveSort()
        }
      }
    }
    recursiveSort()

    /** Insert the item into the sorted list */
    if (currentIndex >= list.length) {
      list = list :+ item
    } else {
      list = list.patch(currentIndex, List(item), 0);
    }
  }

  /** Handles the lesser pending items list.
   *
   * Checks if there's a lesser pending item that can be sorted in.
   * If exists, adds the item to the sorted list at the current index,
   * and the next sort action will be executed against it.
   *
   * @param item Item to be inserted on the `addLesser` method.
   * @param currentIndexItem Current index's item.
   */
  private def handleLesserPendingItems(item: T, currentIndexItem: T): Unit = {
    if (lesserPendingItems.nonEmpty) {
      if (lesserPendingItems.head >= item && lesserPendingItems.head >= currentIndexItem) {
        currentIndex += 1
        list = list.patch(currentIndex, List(lesserPendingItems.head), 0);

        lesserPendingItems = lesserPendingItems.tail
      }
    }
  }

  /** Add to the sorted list an item that's lesser than the current index's item,
   * while performing an action against every item that passes by during the sort process
   *
   * @param item Item to be inserted.
   * @param sortAction Action to apply against every item passes by during the sort process.
   */
  def addLesser(item: T, sortAction: T => Unit): Unit = {

    /** Recursive sort method that keeps looping as long as there are items that are
     * greater/equal to the item that we are sorting in. */
    @tailrec
    def recursiveSort(): Unit = {
      if (currentIndex >= 0) {
        handleLesserPendingItems(item, getCurrentIndexItem)

        if (item <= getCurrentIndexItem) {
          sortAction(getCurrentIndexItem)

          currentIndex -= 1
          recursiveSort()
        }
      }
    }
    recursiveSort()

    /** Insert the item into the sorted list */
    currentIndex += 1
    list = list.patch(currentIndex, List(item), 0);
  }
}
