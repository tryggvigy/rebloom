/* Util functions */
let setAllBitsRightOfLeadingOne (x: int) :int =>
  /* 0b0100 => 0b0111 ; 0b101 => 0b111 */
  List.fold_right (fun a b => b lor b lsr a) [1, 2, 4, 8, 16] x;

let createIndex (h: int) (m: int) :int => {
  let bitmask = setAllBitsRightOfLeadingOne m;
  let index = bitmask land h;
  index
};

/* Hash object and salt */
let hashWithSalt (obj: 'a) (salt: int) :int => Hashtbl.hash (salt + Hashtbl.hash obj);

/* Hash value must map to index value. Hash is much bigger than range of the index (0 to m-1), and
 * this creates a challenge. Taking modulo of hash is one method, but is erroneous b/c it doesn't
 * produce a uniform distribution of index values.
 * Solution is to only take hash values from 0 to m-1. Optimize by masking out bits larger than (m-1).
 * Example, probability of valid hash: if m = 1000,
 * then the odds of a 32 bit hash being less than 'm' is about 1000 / 2^32 */
let rec getIndexes' (obj: 'a) (m: int) (salt: int) (k: int) :list int =>
  switch k {
  | 0 => []
  | _ =>
    let h = hashWithSalt obj salt;
    let index = createIndex h m;
    /* index should be in bounds... i.e. 'm'=10, so valid indexes are 0 to 9 */
    index < m ?
      [index, ...getIndexes' obj m (salt + 1) (k - 1)] : /* Can use index. Decrement k */
      getIndexes' obj m (salt + 1) k /* Can't use index, so try again */
  };

let getIndexes (obj: 'a) (k: int) (m: int) :list int =>
  getIndexes' obj m 0 k; /* Initialize salt value to zero */

let round (x: float) :int => int_of_float (floor (x +. 0.5));

/* Either type */
type either 'a 'b =
  | Left 'a
  | Right 'b;

let getLeft (eith: either 'a 'b) :list 'a =>
  switch eith {
  | Left x => [x]
  | Right y => []
  };

let getRight (eith: either 'a 'b) :list 'b =>
  switch eith {
  | Left x => []
  | Right y => [y]
  };

let fmapEither (fn: 'b => 'c) (eith: either 'a 'b) :either 'a 'c =>
  switch eith {
  | Left x => Left x
  | Right y => Right (fn y)
  };

/* Bloom filter */
module type BF_Functor = {type t;};

module BloomFilter (BF_Functor: {type t;}) => {
  type t = BF_Functor.t;
  type bloomFilterT = {
    n: int /* n  Expected number of elements */,
    p: float /* p  Probability of false positive */,
    k: int /* k  Number of hashes */,
    m: int /* m  Number of bins */,
    bf: BitSet.t /* Bit array (bloom filter) */
  };
  let create (n: int) (p: float) :either string bloomFilterT => {
    let m' = (-1.0) *. float n *. log p /. log 2.0 *\* 2.0;
    let m = round m';
    let k = round (m' /. float n *. log 2.0);
    let bFT = {n, p, m, k, bf: BitSet.create m};
    if (m < 2) {
      Left "m value too small"
    } else if (k < 1) {
      Left "k value less than one"
    } else if (
      p < 0.0 || p > 1.0
    ) {
      Left "p value out of range"
    } else {
      Right bFT
    }
  };
  let insert (b: bloomFilterT) (obj: t) :unit => {
    let bf = b.bf;
    let k = b.k;
    let m = b.m;
    let ks = getIndexes obj k m;
    let setBf = BitSet.set bf;
    List.map setBf ks; /* Map over hash */
    ()
  };
  let test (b: bloomFilterT) (obj: t) :bool => {
    let bf = b.bf;
    let k = b.k;
    let m = b.m;
    let ks = getIndexes obj k m;
    let isSetBf = BitSet.is_set bf;
    let result = List.fold_left (&&) true (List.map isSetBf ks);
    result
  };
};
