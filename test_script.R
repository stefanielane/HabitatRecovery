
# dplyr::summarize(veg_end.n = n(),
#                  veg_end.mean = mean(relabund), 
#                  veg_end.sd = sd(relabund), 
#                  veg_end.se = veg_end.sd/sqrt(veg_end.n))


df = read.table(text = "year	plot	trmt	species
y1	1	X	a
y1	1	X	b
y1	1	X	c
y1	2	X	a
y1	2	X	b
y1	2	X	c
y1	2	X	d
y1	3	X	e
y1	3	X	f
y1	3	X	g
y1	4	Y	a
y1	4	Y	b
y1	4	Y	c
y1	4	Y	d
y1	4	Y	e
y1	5	Y	a
y1	5	Y	b
y1	5	Y	c
y1	5	Y	d
y1	5	Y	e
y1	5	Y	f
y1	5	Y	g
y1	6	Y	a
y1	6	Y	b
y1	6	Y	c
y1	6	Y	d
y1	6	Y	e
y1	6	Y	f
y2	1	X	a
y2	1	X	b
y2	1	X	c
y2	1	X	d
y2	1	X	e
y2	1	X	f
y2	1	X	g
y2	2	X	a
y2	2	X	b
y2	2	X	c
y2	2	X	d
y2	2	X	e
y2	3	X	a
y2	3	X	b
y2	3	X	c
y2	3	X	d
y2	4	Y	a
y2	4	Y	b
y2	4	Y	c
y2	5	Y	a
y2	5	Y	b
y2	5	Y	c
y2	5	Y	d
y2	6	Y	a
y2	6	Y	b
y2	6	Y	c
y2	6	Y	d
              ", header = TRUE)



df2 <- df %>% 
  group_by(year, plot, trmt) %>% 
  mutate(count = n())  
view(df2)

df3 <- df2 %>% 
  group_by(year, trmt) %>% 
  summarize_at(vars(count), funs(mean)) %>% 
view(df3)

df4 %>% df %>% 
  group_by(year, plot, trmt) %>% 
  summarize(count_distinct = n_distinct(species))
view(df4)




#dplyr::summarise(b.div = div.join$count/mean)

df <- data.frame(species = rep(c("A", "B","C", "D", "E", "F"), 6),
                 trmt = rep(c ("X", "X", "X", "Y", "Y", "Y", "Y"), 12),
                 rep = rep(c(2,3,4,1), 9),
                 obs = rep (c("y1", "y2"), 18))         
        

df2 <- df %>% 
  group_by(id) %>% 
  dplyr::summarize(n = n(),
                   sum = sum(count),
                   relabund = count/sum) 

df3 <- cbind(df, "rel.abund" = df2$relabund) %>% 
  pivot_wider(names_from = species, values_from = rel.abund) %>% 
  mutate_all(~replace(., is.na(.), 0))


ra = df3[,6:ncol(df3)]
m.ra = as.matrix(ra)

set.seed(123)
nmds = metaMDS(m.ra, distance = "bray")
nmds

