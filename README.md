# ShowMyData: Repeated Measures – 2 Measures

![ShowMyData Repeated Measures – 2 Measures](images/view.png)

**ShowMyData** is a collection of free, open-source Shiny applications for creating publication-quality data visualizations. Simply copy and paste your data, adjust a few options, and produce elegant graphs suitable for exploration, presentation, or publication.

ShowMyData is built around a simple but surprisingly uncommon design principle: **show the data**.

This application creates publication-quality graphs for paired or repeated-measures data involving two measurements per participant. It emphasizes individual trajectories and within-subject change while preserving access to every observation.

---

## Launch the app

**https://showmydata.org**

---

## Run locally

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "ggridges",
  "ggthemes"
))
```

```r
shiny::runGitHub(
  repo = "smd_repeatedmeasures2",
  username = "ShowMyData",
  subdir = "repeatedmeasures2"
)
```

---

## Download the source code

```r
shiny::runApp("repeatedmeasures2")
```

---

## About ShowMyData

ShowMyData is an open-source collection of interactive Shiny applications that make it easy to create elegant, data-rich visualizations for research, teaching, and publication. Our guiding principle is simple: **show the data**. By making individual observations visible whenever practical, the apps help viewers see what is really present in the data.

Learn more at:

**https://showmydata.org**

---

## Citation

If you use this software in research or teaching, please cite:

> Wilmer, J. B. (2022). *Data Visualization Web Apps* (Version 2.0) [Web Apps]. ShowMyData. https://showmydata.org

---

## License

This software is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0).

---

## Feedback

Bug reports, feature requests, and contributions are welcome through the GitHub Issues page.

---


