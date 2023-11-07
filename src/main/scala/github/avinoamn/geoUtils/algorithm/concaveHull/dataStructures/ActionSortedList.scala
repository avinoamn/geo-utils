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
class ActionSortedList[T](head: T) {

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

  /** Prepend item to `greaterPendingItems`. */
  def addGreaterPendingItem(item: T): Unit = {
    greaterPendingItems = item +: greaterPendingItems
  }

  /** Prepend item to `lesserPendingItems`. */
  def addLesserPendingItem(item: T): Unit = {
    lesserPendingItems = item +: lesserPendingItems
  }

  /** Handles the greater pending items list.
   *
   * Checks if there's a greater pending item that can be sorted in.
   * If exists, adds the item to the sorted list at the current index,
   * and the next sort action will be executed against it.
   *
   * @param item Item to be inserted on the `addGreater` method.
   * @param currentIndexItem Current index's item.
   * @param getComparableItem Method that receive an item, and returns a matching comparable value to sort by.
   * @tparam V The type of the comparable value matching the list's items.
   */
  private def handleGreaterPendingItems[V : Ordering](item: T, currentIndexItem: T, getComparableItem: T => V): Unit = {
    if (greaterPendingItems.nonEmpty) {
      val comparablePendingItem = getComparableItem(greaterPendingItems.head)

      if (comparablePendingItem <= getComparableItem(item) &&
          comparablePendingItem <= getComparableItem(currentIndexItem)) {
        list = list.patch(currentIndex, List(greaterPendingItems.head), 0);

        greaterPendingItems = greaterPendingItems.tail
      }
    }
  }

  /** Add to the sorted list an item that's greater than the current index's item,
   * while performing an action against every item that passes by during the sort process.
   *
   * @param item Item to be inserted.
   * @param getComparableItem Method that receive an item, and returns a matching comparable value to sort by.
   * @param sortAction Action to apply against every item that passes by during the sort process.
   * @tparam V The type of the comparable value matching the list's items.
   */
  def addGreater[V : Ordering](item: T, getComparableItem: T => V, sortAction: T => Unit): Unit = {

    /** Recursive sort method that keeps looping as long as there are items that are
     * lesser/equal to the item that we are sorting in. */
    @tailrec
    def recursiveSort(): Unit = {
      if (currentIndex < list.length) {
        handleGreaterPendingItems(item, getCurrentIndexItem, getComparableItem)

        if (getComparableItem(item) >= getComparableItem(getCurrentIndexItem)) {
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
   * @param getComparableItem Method that receive an item, and returns a matching comparable value to sort by.
   * @tparam V The type of the comparable value matching the list's items.
   */
  private def handleLesserPendingItems[V : Ordering](item: T, currentIndexItem: T, getComparableItem: T => V): Unit = {
    if (lesserPendingItems.nonEmpty) {
      val comparablePendingItem = getComparableItem(lesserPendingItems.head)

      if (comparablePendingItem >= getComparableItem(item) &&
          comparablePendingItem >= getComparableItem(currentIndexItem)) {
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
   * @param getComparableItem Method that receive an item, and returns a matching comparable value to sort by.
   * @param sortAction Action to apply against every item passes by during the sort process.
   * @tparam V The type of the comparable value matching the list's items.
   */
  def addLesser[V : Ordering](item: T, getComparableItem: T => V, sortAction: T => Unit): Unit = {

    /** Recursive sort method that keeps looping as long as there are items that are
     * greater/equal to the item that we are sorting in. */
    @tailrec
    def recursiveSort(): Unit = {
      if (currentIndex >= 0) {
        handleLesserPendingItems(item, getCurrentIndexItem, getComparableItem)

        if (getComparableItem(item) <= getComparableItem(getCurrentIndexItem)) {
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
