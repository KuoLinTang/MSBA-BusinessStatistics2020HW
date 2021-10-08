# Business Statistics Individual Assignment (MSBA 2020)

## The Scenario

You work for a large retailer that sells food and a variety of other products. The retailer has conducted a trial of a new store layout and signage design. It is hoped that this redesign will increase sales. The trial was implemented in roughly half of stores, which were selected at random. You have data on the sales in each store for the last full reporting period prior to the trial being implemented (“sales_1”), and for the first full reporting period after the change was made (“sales_2”). The retailer operates 3 different types of store, the type for each store is indicated in “outlettype”. The variable “intrial” indicates whether the outlet was selected to be in the trial (TRUE) or not (FALSE), and the staff_turnover variable indicates the proportion of staff working
at that outlet that left during the period the data covers.

---
---

## Question 1
Did the new store layout and signage change average sales? Examine this in terms of the change in GPB and the change as a percentage. Which measure do you prefer, and why?

![sales difference in percentage histogram](https://user-images.githubusercontent.com/43996798/136628533-b9099812-95fb-492b-b641-d590772cb976.png)

Figure 1 A histogram of the sales difference of stores. New design stores and ordinary stores are specified by different colours.

Figure 1 is the histogram of sales differences of stores in percentage. From period 1 to period 2, the sales difference of stores in trial increases 1.42% 95% CI[0.85% 1.98%]. The sales difference of stores not in trial only increases 0.1% 95% CI[-0.45% 0.65%]. Then, the new layout increases 1.31% 95% CI[0.53%, 2.1%] of sales difference of stores. This improvement is significant, $t(538) = 3.27$, $p = 0.0011$.

---

I prefer using **percentage data** while practicing ***ANOVA*** to examine the effect of the new layout. Since some of stores generally have much higher sales than the others, only examining the sales difference is insufficient. For example, the store, whose Outlet_ID is *"EhJV"*, has a fewer sales improvement ($577221.4$ GBP) in GBP than the store *"JI92"* ($684439.4$ GBP). Nonetheless, the original sales of the store *"EhJV"* ($6821444$ GBP) is much lower than that of the store *"JI92"* ($15629310$ GBP). Actually, this store is better as it has a higher increasing rate, $8.46%$. Therefore, I prefer percentage measure while performing ANOVA. 

On the other hand, since each row of sales_1 and sales_2 are the multiple measurements from the same outlet, ***repeated measure ANOVA*** is performed to correctly control the link between sales_1 and sales2. In this case, sales differences are not needed, so I simply use the GBP data here.

---
---

## Question 2
Now look in more detail at the effect of the trial on sales. Was there a different effect in different outlet types, and does adding staff turnover as a predictor improve the model?

![sales difference by outlettype and intrial](https://user-images.githubusercontent.com/43996798/136628763-ba0ff1c8-a758-4465-96a2-0f5ca8f51e94.png)

Figure 2. A scatter plot of the sales difference in percentage by different types of outlets. New design stores and ordinary stores are specified by different colours.

Figure 2 presents the sales difference in percentage by different outlet types. There is a strong interaction between intrial and outlettype, $F(2, 534) = 57.08$. That means a different effect in different outlet types does exist. For **community_convenience** outlets, stores in trial get 3.49% 95% CI[2.56% 4.43%] of sales improvement more than stores without any change, $t(534) = 7.33$, $p < 0.0001$. For **superstores**, stores in trial gets 3.48% 95% CI[2% 4.96%] of sales improvement more than stores not in trial, $t(534) = 4.625$, $p < 0.0001$. However, the pattern of **city_centre_convenience** outlets is different. The percentage of sales difference of stores in trial is 4.39% 95% CI[-5.61% -3.18%] **lower** than that of stores not in trial, $t(534) = -7.11$, $p < 0.0001$. Therefore, the effect of the new store design is negative to **city_centre_convenience** outlets.

---

![sales difference by staffturnover and intrial](https://user-images.githubusercontent.com/43996798/136628835-627bc823-9b41-4f41-82f2-a253bfbf93ae.png)

Figure 3. A scatter plot of sales difference by staff turnover rate. New design stores and ordinary stores are specified by different colours.

Figure 3 shows the relationship between sales difference in percentage and staff turnover rate. It seems that the correlation between these two variables is **not significant** as those two lines vary without any pattern. Actually, **staff_turnover** is not a significant predictor of sales difference in the model, $F(1, 537) = 0.1289$, $p = 0.7197$.
