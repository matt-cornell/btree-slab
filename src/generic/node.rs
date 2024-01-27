use std::{borrow::Borrow, cmp::Ordering, fmt};

mod addr;
pub mod internal;
mod item;
mod leaf;

pub use addr::Address;
pub use internal::Internal as InternalNode;
pub use item::Item;
pub use leaf::Leaf as LeafNode;

/// Type identifier by a key.
///
/// This is implemented by [`Item`] and [`internal::Branch`].
pub trait Keyed {
	type Key;

	fn key(&self) -> &Self::Key;
}

/// Offset in a node.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Offset(usize);

impl Offset {
	pub fn before() -> Offset {
		Offset(usize::MAX)
	}

	pub fn is_before(&self) -> bool {
		self.0 == usize::MAX
	}

	pub fn value(&self) -> Option<usize> {
		if self.0 == usize::MAX {
			None
		} else {
			Some(self.0)
		}
	}

	pub fn unwrap(self) -> usize {
		if self.0 == usize::MAX {
			panic!("Offset out of bounds")
		} else {
			self.0
		}
	}

	pub fn incr(&mut self) {
		if self.0 == usize::MAX {
			self.0 = 0
		} else {
			self.0 += 1
		}
	}

	pub fn decr(&mut self) {
		if self.0 == 0 {
			self.0 = usize::MAX
		} else {
			self.0 -= 1
		}
	}
}

impl PartialOrd for Offset {
	fn partial_cmp(&self, offset: &Offset) -> Option<Ordering> {
		if self.0 == usize::MAX || offset.0 == usize::MAX {
			if self.0 == usize::MAX && offset.0 == usize::MAX {
				Some(Ordering::Equal)
			} else if self.0 == usize::MAX {
				Some(Ordering::Less)
			} else {
				Some(Ordering::Greater)
			}
		} else {
			self.0.partial_cmp(&offset.0)
		}
	}
}

impl Ord for Offset {
	fn cmp(&self, offset: &Offset) -> Ordering {
		if self.0 == usize::MAX || offset.0 == usize::MAX {
			if self.0 == usize::MAX && offset.0 == usize::MAX {
				Ordering::Equal
			} else if self.0 == usize::MAX {
				Ordering::Less
			} else {
				Ordering::Greater
			}
		} else {
			self.0.cmp(&offset.0)
		}
	}
}

impl PartialEq<usize> for Offset {
	fn eq(&self, offset: &usize) -> bool {
		self.0 != usize::MAX && self.0 == *offset
	}
}

impl PartialOrd<usize> for Offset {
	fn partial_cmp(&self, offset: &usize) -> Option<Ordering> {
		if self.0 == usize::MAX {
			Some(Ordering::Less)
		} else {
			self.0.partial_cmp(offset)
		}
	}
}

impl From<usize> for Offset {
	fn from(offset: usize) -> Offset {
		Offset(offset)
	}
}

impl fmt::Display for Offset {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.0 == usize::MAX {
			write!(f, "-1")
		} else {
			self.0.fmt(f)
		}
	}
}

impl fmt::Debug for Offset {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		if self.0 == usize::MAX {
			write!(f, "-1")
		} else {
			self.0.fmt(f)
		}
	}
}

/// Node balance.
#[derive(Debug)]
pub enum Balance {
	/// The node is balanced.
	Balanced,

	/// The node is overflowing.
	Overflow,

	/// The node is underflowing.
	///
	/// The boolean is `true` if the node is empty.
	Underflow(bool),
}

/// Error returned when an operation on the node would result in an underflow.
pub struct WouldUnderflow;

/// Type of the value returned by `Node::pop_right`.
///
/// It includes the offset of the popped item, the item itself and the index of
/// the right child of the item if it is removed from an internal node.
pub type PoppedItem<K, V> = (Offset, Item<K, V>, Option<usize>);

/// B-tree node.
#[derive(Clone)]
pub enum Node<K, V> {
	/// Internal node.
	Internal(InternalNode<K, V>),

	/// Leaf node.
	Leaf(LeafNode<K, V>),
}

impl<K, V> Node<K, V> {
	#[inline]
	pub fn binary(
		parent: Option<usize>,
		left_id: usize,
		median: Item<K, V>,
		right_id: usize,
	) -> Node<K, V> {
		Node::Internal(InternalNode::binary(parent, left_id, median, right_id))
	}

	#[inline]
	pub fn leaf(parent: Option<usize>, item: Item<K, V>) -> Node<K, V> {
		Node::Leaf(LeafNode::new(parent, item))
	}

	#[inline]
	pub fn balance(&self) -> Balance {
		match self {
			Node::Internal(node) => node.balance(),
			Node::Leaf(leaf) => leaf.balance(),
		}
	}

	#[inline]
	pub fn is_underflowing(&self) -> bool {
		match self {
			Node::Internal(node) => node.is_underflowing(),
			Node::Leaf(leaf) => leaf.is_underflowing(),
		}
	}

	#[inline]
	pub fn is_overflowing(&self) -> bool {
		match self {
			Node::Internal(node) => node.is_overflowing(),
			Node::Leaf(leaf) => leaf.is_overflowing(),
		}
	}

	#[inline]
	pub fn parent(&self) -> Option<usize> {
		match self {
			Node::Internal(node) => node.parent(),
			Node::Leaf(leaf) => leaf.parent(),
		}
	}

	#[inline]
	pub fn set_parent(&mut self, p: Option<usize>) {
		match self {
			Node::Internal(node) => node.set_parent(p),
			Node::Leaf(leaf) => leaf.set_parent(p),
		}
	}

	#[inline]
	pub fn item_count(&self) -> usize {
		match self {
			Node::Internal(node) => node.item_count(),
			Node::Leaf(leaf) => leaf.item_count(),
		}
	}

	#[inline]
	pub fn child_count(&self) -> usize {
		match self {
			Node::Internal(node) => node.child_count(),
			Node::Leaf(_) => 0,
		}
	}

	#[inline]
	pub fn child_index(&self, id: usize) -> Option<usize> {
		match self {
			Node::Internal(node) => node.child_index(id),
			_ => None,
		}
	}

	#[inline]
	pub fn child_id(&self, index: usize) -> usize {
		match self {
			Node::Internal(node) => node.child_id(index),
			_ => panic!("only internal nodes can be indexed"),
		}
	}

	#[inline]
	pub fn child_id_opt(&self, index: usize) -> Option<usize> {
		match self {
			Node::Internal(node) => node.child_id_opt(index),
			Node::Leaf(_) => None,
		}
	}

	#[inline]
	pub fn get<Q: ?Sized, F: Fn(&Q, &Q) -> Ordering>(
		&self,
		key: &Q,
		cmp: &F,
	) -> Result<Option<&V>, usize>
	where
		K: Borrow<Q>,
	{
		match self {
			Node::Leaf(leaf) => Ok(leaf.get(key, cmp)),
			Node::Internal(node) => match node.get(key, cmp) {
				Ok(value) => Ok(Some(value)),
				Err(e) => Err(e),
			},
		}
	}

	#[inline]
	pub fn get_mut<Q: ?Sized, F: Fn(&Q, &Q) -> Ordering>(
		&mut self,
		key: &Q,
		cmp: &F,
	) -> Result<Option<&mut V>, usize>
	where
		K: Borrow<Q>,
	{
		match self {
			Node::Leaf(leaf) => Ok(leaf.get_mut(key, cmp)),
			Node::Internal(node) => match node.get_mut(key, cmp) {
				Ok(value) => Ok(Some(value)),
				Err(e) => Err(e),
			},
		}
	}

	/// Find the offset of the item matching the given key.
	///
	/// If the key matches no item in this node,
	/// this funtion returns the index and id of the child that may match the key,
	/// or `Err(None)` if it is a leaf.
	#[inline]
	pub fn offset_of<Q: ?Sized, F: Fn(&Q, &Q) -> Ordering>(
		&self,
		key: &Q,
		cmp: &F,
	) -> Result<Offset, (usize, Option<usize>)>
	where
		K: Borrow<Q>,
	{
		match self {
			Node::Internal(node) => match node.offset_of(key, cmp) {
				Ok(i) => Ok(i),
				Err((index, child_id)) => Err((index, Some(child_id))),
			},
			Node::Leaf(leaf) => match leaf.offset_of(key, cmp) {
				Ok(i) => Ok(i),
				Err(index) => Err((index.unwrap(), None)),
			},
		}
	}

	#[inline]
	pub fn item(&self, offset: Offset) -> Option<&Item<K, V>> {
		match self {
			Node::Internal(node) => node.item(offset),
			Node::Leaf(leaf) => leaf.item(offset),
		}
	}

	#[inline]
	pub fn item_mut(&mut self, offset: Offset) -> Option<&mut Item<K, V>> {
		match self {
			Node::Internal(node) => node.item_mut(offset),
			Node::Leaf(leaf) => leaf.item_mut(offset),
		}
	}

	/// Insert by key.
	///
	/// It is assumed that the node is not free.
	/// If it is a leaf node, there must be a free space in it for the inserted value.
	#[inline]
	pub fn insert_by_key<F: Fn(&K, &K) -> Ordering>(
		&mut self,
		key: K,
		value: V,
		cmp: &F,
	) -> Result<(Offset, Option<V>), internal::InsertionError<K, V>> {
		match self {
			Node::Internal(node) => match node.insert_by_key(key, value, cmp) {
				Ok((offset, value)) => Ok((offset, Some(value))),
				Err(e) => Err(e),
			},
			Node::Leaf(leaf) => Ok(leaf.insert_by_key(key, value, cmp)),
		}
	}

	/// Split the node.
	/// Return the length of the node after split, the median item and the right node.
	#[inline]
	pub fn split(&mut self) -> (usize, Item<K, V>, Node<K, V>) {
		match self {
			Node::Internal(node) => {
				let (len, item, right_node) = node.split();
				(len, item, Node::Internal(right_node))
			}
			Node::Leaf(leaf) => {
				let (len, item, right_leaf) = leaf.split();
				(len, item, Node::Leaf(right_leaf))
			}
		}
	}

	#[inline]
	pub fn merge(
		&mut self,
		left_index: usize,
		right_index: usize,
	) -> (usize, usize, usize, Item<K, V>, Balance) {
		match self {
			Node::Internal(node) => node.merge(left_index, right_index),
			_ => panic!("only internal nodes can merge children"),
		}
	}

	/// Return the offset of the separator.
	#[inline]
	pub fn append(&mut self, separator: Item<K, V>, other: Node<K, V>) -> Offset {
		match (self, other) {
			(Node::Internal(node), Node::Internal(other)) => node.append(separator, other),
			(Node::Leaf(leaf), Node::Leaf(other)) => leaf.append(separator, other),
			_ => panic!("incompatibles nodes"),
		}
	}

	#[inline]
	pub fn push_left(&mut self, item: Item<K, V>, opt_child_id: Option<usize>) {
		match self {
			Node::Internal(node) => node.push_left(item, opt_child_id.unwrap()),
			Node::Leaf(leaf) => leaf.push_left(item),
		}
	}

	#[inline]
	pub fn pop_left(&mut self) -> Result<(Item<K, V>, Option<usize>), WouldUnderflow> {
		match self {
			Node::Internal(node) => {
				let (item, child_id) = node.pop_left()?;
				Ok((item, Some(child_id)))
			}
			Node::Leaf(leaf) => Ok((leaf.pop_left()?, None)),
		}
	}

	#[inline]
	pub fn push_right(&mut self, item: Item<K, V>, opt_child_id: Option<usize>) -> Offset {
		match self {
			Node::Internal(node) => node.push_right(item, opt_child_id.unwrap()),
			Node::Leaf(leaf) => leaf.push_right(item),
		}
	}

	#[inline]
	pub fn pop_right(&mut self) -> Result<PoppedItem<K, V>, WouldUnderflow> {
		match self {
			Node::Internal(node) => {
				let (offset, item, child_id) = node.pop_right()?;
				Ok((offset, item, Some(child_id)))
			}
			Node::Leaf(leaf) => {
				let (offset, item) = leaf.pop_right()?;
				Ok((offset, item, None))
			}
		}
	}

	#[inline]
	pub fn leaf_remove(&mut self, offset: Offset) -> Option<Result<Item<K, V>, usize>> {
		match self {
			Node::Internal(node) => {
				if offset < node.item_count() {
					let left_child_index = offset.unwrap();
					Some(Err(node.child_id(left_child_index)))
				} else {
					None
				}
			}
			Node::Leaf(leaf) => {
				if offset < leaf.item_count() {
					Some(Ok(leaf.remove(offset)))
				} else {
					None
				}
			}
		}
	}

	#[inline]
	pub fn remove_rightmost_leaf(&mut self) -> Result<Item<K, V>, usize> {
		match self {
			Node::Internal(node) => {
				let child_index = node.child_count() - 1;
				let child_id = node.child_id(child_index);
				Err(child_id)
			}
			Node::Leaf(leaf) => Ok(leaf.remove_last()),
		}
	}

	/// Put an item in a node.
	///
	/// It is assumed that the node will not overflow.
	#[inline]
	pub fn insert(&mut self, offset: Offset, item: Item<K, V>, opt_right_child_id: Option<usize>) {
		match self {
			Node::Internal(node) => node.insert(offset, item, opt_right_child_id.unwrap()),
			Node::Leaf(leaf) => leaf.insert(offset, item),
		}
	}

	#[inline]
	pub fn replace(&mut self, offset: Offset, item: Item<K, V>) -> Item<K, V> {
		match self {
			Node::Internal(node) => node.replace(offset, item),
			_ => panic!("can only replace in internal nodes"),
		}
	}

	#[inline]
	pub fn separators(&self, i: usize) -> (Option<&K>, Option<&K>) {
		match self {
			Node::Leaf(_) => (None, None),
			Node::Internal(node) => node.separators(i),
		}
	}

	#[inline]
	pub fn children(&self) -> Children<K, V> {
		match self {
			Node::Leaf(_) => Children::Leaf,
			Node::Internal(node) => node.children(),
		}
	}

	#[inline]
	pub fn children_with_separators(&self) -> ChildrenWithSeparators<K, V> {
		match self {
			Node::Leaf(_) => ChildrenWithSeparators::Leaf,
			Node::Internal(node) => node.children_with_separators(),
		}
	}
}

pub enum Children<'a, K, V> {
	Leaf,
	Internal(Option<usize>, std::slice::Iter<'a, internal::Branch<K, V>>),
}

impl<'a, K, V> Iterator for Children<'a, K, V> {
	type Item = usize;

	#[inline]
	fn next(&mut self) -> Option<usize> {
		match self {
			Children::Leaf => None,
			Children::Internal(first, rest) => match first.take() {
				Some(child) => Some(child),
				None => rest.next().map(|branch| branch.child),
			},
		}
	}
}

pub enum ChildrenWithSeparators<'a, K, V> {
	Leaf,
	Internal(
		Option<usize>,
		Option<&'a Item<K, V>>,
		std::iter::Peekable<std::slice::Iter<'a, internal::Branch<K, V>>>,
	),
}

impl<'a, K, V> Iterator for ChildrenWithSeparators<'a, K, V> {
	type Item = (Option<&'a Item<K, V>>, usize, Option<&'a Item<K, V>>);

	#[inline]
	fn next(&mut self) -> Option<Self::Item> {
		match self {
			ChildrenWithSeparators::Leaf => None,
			ChildrenWithSeparators::Internal(first, left_sep, rest) => match first.take() {
				Some(child) => {
					let right_sep = rest.peek().map(|right| &right.item);
					*left_sep = right_sep;
					Some((None, child, right_sep))
				}
				None => match rest.next() {
					Some(branch) => {
						let right_sep = rest.peek().map(|right| &right.item);
						let result = Some((*left_sep, branch.child, right_sep));
						*left_sep = right_sep;
						result
					}
					None => None,
				},
			},
		}
	}
}
