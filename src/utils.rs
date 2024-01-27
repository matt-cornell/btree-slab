use crate::generic::node::Keyed;
use std::borrow::Borrow;
use std::cmp::Ordering;

/// Search in `sorted_slice` for the item with the nearest key smaller or equal to the given one.
///
/// `sorted_slice` is assumed to be sorted.
#[inline]
pub fn binary_search_min<T: Keyed, Q: ?Sized, F: Fn(&Q, &Q) -> Ordering>(
	sorted_slice: &[T],
	key: &Q,
	cmp: &F,
) -> Option<usize>
where
	T::Key: Borrow<Q>,
{
	if sorted_slice.is_empty() || cmp(sorted_slice[0].key().borrow(), key) == Ordering::Greater {
		None
	} else {
		let mut i = 0;
		let mut j = sorted_slice.len() - 1;

		if cmp(sorted_slice[j].key().borrow(), key) != Ordering::Greater {
			return Some(j);
		}

		// invariants:
		// sorted_slice[i].key <= key
		// sorted_slice[j].key > key
		// j > i

		while j - i > 1 {
			let k = (i + j) / 2;

			if cmp(sorted_slice[k].key().borrow(), key) == Ordering::Greater {
				j = k;
			// sorted_slice[k].key > key --> sorted_slice[j] > key
			} else {
				i = k;
				// sorted_slice[k].key <= key --> sorted_slice[i] <= key
			}
		}

		Some(i)
	}
}
