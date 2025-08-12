test_that("subgrouping works on the simple example from the article", {
  subgroups_res = method1(ghosh_table_3$D_i, ghosh_table_3$Subject)

  expect_equal(subgroups_res$subgroups_idx, ghosh_table_3$True_Subgroup)
})

test_that("subgrouping works when there are ties", {
  subgroups_res = method1(heartburn_multi_center$D, heartburn_multi_center$ID)

  # TODO: compute the true subgroups

  # expect_equal(subgroups_res$subgroups_idx, ghosh_table_3$True_Subgroup)
})

test_that("subgrouping works when there are NAs", {
  
})