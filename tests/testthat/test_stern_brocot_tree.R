test_that('the tree is properly created for depth 0', {
  t = stern_brocot_tree(0)
  expect_equal(t$num,c(-1,0,1))
  expect_equal(t$den,c( 0,1,0))
  expect_equal(t$label,c("-1/0","0/1","1/0"))
  expect_equal(t$depth,c(-1,0,-1))
  expect_equal(t$left_parent,c("","-1/0",""))
  expect_equal(t$right_parent,c("","1/0",""))
})
test_that('the tree is properly created for depth 1', {
  t = stern_brocot_tree(1)
  expect_equal(t$num,c(-1,-1,0,1,1))
  expect_equal(t$den,c( 0,1,1,1,0))
  expect_equal(t$label,c("-1/0","-1/1","0/1","1/1","1/0"))
  expect_equal(t$depth,c(-1,1,0,1,-1))
  expect_equal(t$left_parent,c("","-1/0","-1/0","0/1",""))
  expect_equal(t$right_parent,c("", "0/1", "1/0", "1/0", ""))
})
test_that('the tree is properly created for depth 2', {
  t = stern_brocot_tree(2)
  expect_equal(t$num,c(-1,-2,-1,-1,0,1,1,2,1))
  expect_equal(t$den,c( 0,1,1,2,1,2,1,1,0))
  expect_equal(t$depth,c(-1,2,1,2,0,2,1,2,-1))
  expect_equal(t$label,c("-1/0","-2/1","-1/1","-1/2","0/1","1/2","1/1","2/1","1/0"))
  expect_equal(t$left_parent,c("","-1/0","-1/0","-1/1","-1/0","0/1","0/1","1/1",""))
  expect_equal(t$right_parent,c("", "-1/1","0/1", "0/1", "1/0", "1/1", "1/0", "1/0", ""))
})
test_that('the tree has no duplicates at depth 4', {

  depth = 4
  t = stern_brocot_tree(depth)

  expect_equal(nrow(t), 2^(depth+1)+1)

  # Create a unique data frame of (num, den) pairs
  unique_pairs <- unique(t[, c("num", "den")])

  # Expect that the number of unique pairs matches the total rows in the tree
  expect_equal(nrow(unique_pairs), nrow(t),
               info = "Duplicate (num, den) pairs found in the tree")
})
test_that('the tree has no duplicates at depth 7', {
  depth = 7
  t = stern_brocot_tree(depth)

  expect_equal(nrow(t), 2^(depth+1)+1)

  # Create a unique data frame of (num, den) pairs
  unique_pairs <- unique(t[, c("num", "den")])

  # Expect that the number of unique pairs matches the total rows in the tree
  expect_equal(nrow(unique_pairs), nrow(t),
               info = "Duplicate (num, den) pairs found in the tree")
})
