#** Analysis on the 1) LeseaLch question
###
by_unit = group_by(cleaned_data, unit)
avarage_learning = summarise(by_unit,
                             L_var1 = round(mean(L_var1, na.rm = TRUE), 0),
                             L_var2 = round(mean(L_var2, na.rm = TRUE), 0),
                             L_var3 = round(mean(L_var3, na.rm = TRUE), 0),
                             L_var4 = round(mean(L_var4, na.rm = TRUE), 0),
                             L_var5 = round(mean(L_var5, na.rm = TRUE), 0),
                             L_var6 = round(mean(L_var6, na.rm = TRUE), 0),
                             L_var7 = round(mean(L_var7, na.rm = TRUE), 0),
                             L_var8 = round(mean(L_var8, na.rm = TRUE), 0),
                             L_var9 = round(mean(L_var9, na.rm = TRUE), 0),
                             L_var10 = round(mean(L_var10, na.rm = TRUE), 0),
                             L_var11 = round(mean(L_var11, na.rm = TRUE), 0),
                             L_var12 = round(mean(L_var12, na.rm = TRUE), 0),
                             L_var13 = round(mean(L_var13, na.rm = TRUE), 0),
                             Count = n())

avarage_learning <- avarage_learning %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(L_var1,L_var2,L_var3,L_var4,L_var5,L_var6,L_var7,L_var8,L_var9,L_var10,L_var11,L_var12,L_var13)),0))

attach(avarage_learning)
avarage_learning = avarage_learning %>%
  add_row(
    unit = "Mean",
    L_var1 = round(mean(L_var1), 0),
    L_var2 = round(mean(L_var2), 0),
    L_var3 = round(mean(L_var3), 0),
    L_var4 = round(mean(L_var4), 0),
    L_var5 = round(mean(L_var5), 0),
    L_var6 = round(mean(L_var6), 0),
    L_var7 = round(mean(L_var7), 0),
    L_var8 = round(mean(L_var8), 0),
    L_var9 = round(mean(L_var9), 0),
    L_var10 = round(mean(L_var10), 0),
    L_var11 = round(mean(L_var11), 0),
    L_var12 = round(mean(L_var12), 0),
    L_var13 = round(mean(L_var13), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )
View(avarage_learning)
