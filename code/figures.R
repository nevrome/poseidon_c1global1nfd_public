library(magrittr)
library(ggplot2)

#### read context data ####

janno <- janno::read_janno("../c1global1nfd_public/c1global1nfd_public.janno")

#### pca ####

system("trident genoconvert -d ../c1global1nfd_public --outFormat EIGENSTRAT -o tmp")

pca_out <- smartsnp::smart_pca(
  "tmp/c1global1nfd_public.geno",
  sample_group = seq_len(nrow(janno)),
  missing_impute = "mean",
  pc_axes = 2
)

# extract the explained variance percentages for axis labels
var_expl <- pca_out$pca.eigenvalues["variance explained",]
xlab <- sprintf("PC1 (%.1f%%)", var_expl[1])
ylab <- sprintf("PC2 (%.1f%%)", var_expl[2])

#### plot pca & map ####

png("plot.png", width = 1600, height = 800, res = 150)
par(mfrow = c(1, 2), mar = c(1.5, 0, 0.5, 0.5)) # 1 row, 2 columns

# map
globe::globeearth(eye = list(
  mean(janno$Longitude, na.rm = T),
  mean(janno$Latitude, na.rm = T)
))
globe::globepoints(
  loc = janno[,c("Longitude", "Latitude")] %>% as.matrix(),
  col = janno$Group_Color,
  cex = 1,
  pch = 20
)

# pca
plot(
  pca_out$pca.sample_coordinates$PC1,
  pca_out$pca.sample_coordinates$PC2,
  col = janno$Group_Color,
  xlab = xlab,
  ylab = ylab,
  pch = 20,
  asp = 1,
  mgp = c(-1.2, 0.5, 0),
  adj = 0.1
)

dev.off()

