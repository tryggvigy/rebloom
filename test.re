open OUnit2;

open Bloom_filter;

let failsIfInitializedWithLessThanTwoExpectedItemsTest _ => {
  module S = BloomFilter String;
  assert_string (
    switch (S.create 100 0.1) {
    | Left x => x
    | Right _ => ""
    }
  )
};

let stringCheckOnEmptySetTest _ => {
  module S = BloomFilter String;
  let bf = List.hd (getRight (S.create 100 0.1));
  assert_equal (S.test bf "hey") false
};

let stringInSetCheckTest _ => {
  module S = BloomFilter String;
  let bf = List.hd (getRight (S.create 100 0.1));
  assert_equal (S.test bf "hey") false;
  S.insert bf "hey";
  assert_equal (S.test bf "hey") true
};

let suite =
  "BloomFilter" >::: [
    "give error message if initialized with less than two expected items" >:: failsIfInitializedWithLessThanTwoExpectedItemsTest,
    "reports existance correctly on an empty set" >:: stringCheckOnEmptySetTest,
    "reports without a doubt that seen string is in set" >:: stringInSetCheckTest
  ];

let () = run_test_tt_main suite;
