depths = read.csv('csv/model_depths_at_obs_locations.csv')
meta   = read.csv('csv/all_metadata_adjusted.csv')
meta$loc = substr(meta$name, 1, 5)
depths = merge(meta,depths)
rownames(depths) = depths$name


get_dz_lake = function(depth) {
  z = rep(NA,10)
  if(depth<=1) {z = rep(depth/10,10)}
  else {z[1] = 0.1; z[2:10] = rep((depth-0.1)/9,9)}
  return(z)
}

get_z_lake = function(depth) {
  dz = get_dz_lake(depth)
  z = rep(NA, 10)
  z[1] = dz[1]/2
  for (i in 2:10) {z[i] = sum(dz[1:(i-1)], dz[i]/2)}
  return(z)
}

z = array(NA, dim = c(nrow(depths), 10, 3), dimnames = list(depths$name, 1:10, c("ctl", "bi0m", "bi2m")))
dz = array(NA, dim = c(nrow(depths), 10, 3), dimnames = list(depths$name, 1:10, c("ctl", "bi0m", "bi2m")))

for (name in dimnames(z)[[1]]) {
  for (mod in dimnames(z)[[3]]) {
    z[name, , mod] = get_z_lake(depths[name, mod])
    dz[name, , mod] = get_dz_lake(depths[name, mod])
  }
}

