<<<<<<< HEAD
# BIOS 611 Final Project

## King County Housing Sales Data Analysis: What factors influence house prices?

##### Gayoung Lee (MPH 2nd-year student, Applied Epidemiology)

##### 12/13/2023

### Dataset:

House Sales in King County, USA <https://www.kaggle.com/datasets/harlfoxem/housesalesprediction> From Kaggle.

I aimed to explore the significant factors influencing house prices in King County during 2014-2015 using the house sales dataset.

I intended to conduct an analysis that incorporates geographic information utilizing latitude and longitude, along with information about the houses themselves, such as the year built, number of bedrooms, and grade. This comprehensive analysis seeks to understand the various elements affecting house prices during the specified period.

### Build docker container:

1.  Turn on the docker desktop.

2.  Build the docker container and run the development environment.

```         
cd "/Users/gayoungl/Documents/GitHub/bios611"
docker build -t bios611 .
```

Create a docker container.

```         
docker run -v $(pwd):/home/rstudio/ -e PASSWORD=yourpassword --rm -p 8787:8787 bios611
```

Use the browser <http://localhost:8787> on your machine to access the development environment.

3.  Run the following command on the terminal.

```         
make report.html
```

The report will be automatically made.

If you want to clean the output, run the code below:
```{}
make clean
```