
# NBA Players Payroll 2017/18 Explainer

Dashboard built in Shiny, in which, using mainly [DALEX](https://github.com/ModelOriented/DALEX) package we summarize the data and explain what are the factors contributing to certain players' salary.

## tl;dr [See the app now!](http://3.21.186.80:3838/nba-payroll/) 
*Link above is no longer available, but there are visualisations below.*

<br>

---

<br>


## App overview


- [components](https://github.com/thormic/nba-payroll/tree/master/components)
  - [body.R](https://github.com/thormic/nba-payroll/blob/master/components/body.R)
  - [header.R](https://github.com/thormic/nba-payroll/blob/master/components/header.R)
  - [sidebar.R](https://github.com/thormic/nba-payroll/blob/master/components/sidebar.R)
- [data](https://github.com/thormic/nba-payroll/tree/master/data)
  - [NBA dataset](https://github.com/thormic/nba-payroll/blob/master/data/nba_dataset.csv)
  - [NBA workspace in .RData](https://github.com/thormic/nba-payroll/blob/master/data/nba_workspace.RData)
- [app.R](https://github.com/thormic/nba-payroll/blob/master/app.R)
- [global.R](https://github.com/thormic/nba-payroll/blob/master/global.R)
- [server.R](https://github.com/thormic/nba-payroll/blob/master/server.R)
- [ui.R](https://github.com/thormic/nba-payroll/blob/master/ui.R)

<br>

---

<br>


## Dashboard walkthrough


**Data summary**

* In the first tab you can get familiar with the goal of the dashboard, check how the dataset looks like, understand variables or see histograms of any variables that you like. 

![Recordit GIF](http://g.recordit.co/rANyZZQloL.gif)

---

**Model comparison**

* Here are visualised the differences between models that were used to predict NBA Players salaries.

![Recordit GIF](http://g.recordit.co/l3X1kkq4dd.gif)

---

**Team**

* First, select the team. Then you will see all the information about that team. You can also catch up on this team players on the bottom of the tab. You can change the selected team at any time.

![Recordit GIF](http://g.recordit.co/X58B1XuGTT.gif)

---

**Player**

* Last tab is dedicated to individual players. Once you have chosen the player, you will see basic information about him. On *ceteris paribus* plots you can see given variable, level of that variable presented by chosen player and how (assuming no other variables are changed) different values of that variable would influence his salary.

* On the bottom of the page you can **compare players** with the chosen one. It is up to you, if you want to compare 1 or 2 players with him and using *GBM* or *Random Forest* model. The *Breakdown* for each players shows how much each variable influenced his salary and what is his predicted salary.

![Recordit GIF](http://g.recordit.co/p6ZvZL0U2P.gif)

<br>

---

<br>


## Further research

- [x] Player comparison between up to 3 players
- [ ] Enable usage of different regression models
- [ ] Enable continous scrapping of the [data source](https://www.basketball-reference.com/), so the data is always up to date
- [ ] Add team logos and player photos
- [ ] Add data manipulation from dashboard
- [ ] Create your own player tab


<br>

---

<br>


## Clone

- Clone this repo to your local machine using 
```bash
git clone https://github.com/thormic/nba-payroll.git
```
<br>

---

<br>


## Contributing

- Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

<br>

---

<br>

## Team

| <a href="" target="_blank">**thormic**</a> | <a href="" target="_blank">**blawok**</a> |
| :---: |:---:| 
| [![thormic](https://avatars3.githubusercontent.com/u/46217631?s=200&u=c4c2b5c653a1837798f4375b36e05ed9dc4bb7c2&v=4?s=200)](http://fvcproductions.com)    | [![blawok](https://avatars3.githubusercontent.com/u/41793223?s=200&u=d08c3e7b559c048747e321c5997eb9a2fb99882d&v=4)](http://fvcproductions.com) |
| <a href="https://github.com/thormic" target="_blank">`github.com/thormic`</a> | <a href="https://github.com/blawok" target="_blank">`github.com/blawok`</a> |

<br>

---

<br>


## License

- **[MIT license](http://opensource.org/licenses/mit-license.php)**
