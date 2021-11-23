## Characterizing and simulating the movement of late instar *Lymantria dispar* (Lepidoptera: Erebidae) to evaluate the effectiveness of regulatory practices

This is the first project I worked on as a Master's student in Entomology at the University of Minnesota. I cut my teeth in R on this project and you can certainly tell that in the code. Both in terms of how I organized my data, scripts, and code and how I was programming at the time. When I first started on this project, I had not done anything in R before. By the time I was done with the project, I was much more proficient with data cleaning, analysis, and visualizing. In these scripts you'll find code for:

- Cleaning movement data recorded in polar coordinates and deriving associated variables
- Exploratory data visualiztion, which I used to get a feel for my data
- Statistical tests including ANOVA and generalized linear models, along with statistical techniques used with circular data
- A Monte Carlo simulation using parameters derived from my experiments to simulate the movement of *L. dispar* caterpillars
- Publication quality plots of the results I presented in my thesis (in the process of publishing these chapters)

The paper can be found here: https://academic.oup.com/ee/advance-article/doi/10.1093/ee/nvz025/5429494

### Background 
*L. dispar*  is an invasive insect in North America. It was introduced to the United States in the 1860s and has since been spreading across the country. *Lymatnria dispar*  are easily spread by moving their egg masses. Adult female *L. dispar* are flightless and will lay eggs indiscriminately, which may include the underside of cars or firewood. There is a quarantine in place, maintained by the USDA, to limit the spread of the *L. dispar* . One of the practices in the quarantine used to limit the spread of *L. dispar*  targets industries that move wood products across the quarantine boundary. These industries must "stage" their wood products in a 100-foot radius, host vegetation-free buffer zone. We didn't know much about how *L. dispar*  caterpillars moved in such buffer zone environments until I completed this project. This lack of knowledge made it difficult to evaluate how effective the buffer zone practice was. In this project, we released *L. dispar*  caterpillars in a buffer zone environment and tracked their movement with harmonic radar. We looked at how the presence or absence of vegetation on the edge of the buffer zone and starvation affected the movement of the caterpillars. 

### Conclusion Highlights

1) **Caterpillars are able to reach products staged at the center of a 100-foot buffer zone but appear unlikely to do so.** When no host vegetation was present, 4% of the caterpillars we released reached the wood staged at the center of the buffer zone. No caterpillars that we released in the presence of host vegetation reached the center of the buffer zone.
2) **Increasing the size of the buffer zone will decrease the likelihood caterpillars will reach objects staged at the center, but with diminishing returns.** According to our simulation models of different buffer zone sizes, increases in the efficacy of the buffer zone diminish quickly after a 125-foot radius. This result is, however, dependent on the size of the objects staged at the center of the buffer zone. Larger staging areas may require larger buffer zones to stay effective.
3) **The buffer zone practice is likely achieving the desired impact.** Our simulation and field studies show small numbers of *L. dispar* caterpillars will reach the center of the buffer zone. Under conditions where hosts are available on the outside of the buffer zone and populations of *L. dispar* are low, it is unlikely that caterpillars will enter the buffer zone.





