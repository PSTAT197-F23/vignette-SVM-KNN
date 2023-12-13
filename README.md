# Vignette: XGBoost and Support Vector Machine

Vignette on fitting and Support Vector Machine and XGBoost to heart data; created as a final project for PSTAT197A for Fall 2023.

### Contributors

Lu Liu, Haoran Yan, Shuai Yuan, Irena Wong, and Molata Fu

## Abstract

Through this project, we aim to implement support vector machines and XGBoost in order to predict the likelihood of a patient getting a heart attack. This is a binary classification problem with the outcome categories: less chance of heart attack and more chance of heart attack.

#### Our Data

This is the Heart Attack Analysis & Prediction Dataset by Rashik Rahman on Kaggle. The dataset has 303 entries and the following variables--

-   **Age :** Age of the patient

-   **Sex :** Sex of the patient

-   **exang:** exercise induced angina (1 = yes; 0 = no)

-   **ca:** number of major vessels (0-3)

-   **cp :** Chest Pain type chest pain type

    -   Value 1: typical angina

    -   Value 2: atypical angina

    -   Value 3: non-anginal pain

    -   Value 4: asymptomatic

-   **trtbps :** resting blood pressure (in mm Hg)

-   **chol :** cholestoral in mg/dl fetched via BMI sensor

-   **fbs :** (fasting blood sugar \> 120 mg/dl) (1 = true; 0 = false)

-   **rest_ecg :** resting electrocardiographic results

    -   Value 0: normal

    -   Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of \> 0.05 mV)

    -   Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria

-   **thalach :** maximum heart rate achieved

-   **target :** 0= less chance of heart attack 1= more chance of heart attack

Our outcome variable is *target*, which classifies the chance that a subject will get a heart attack.

## Repository Contents

Structure of this repository:

```         
root directory
├── data
   ├── raw
   ├── preprocessed
├── scripts
   ├── drafts
   ├── EDA.R
   ├── model-fitting.R
├── results
   ├── vignette_files
      ├── figure-html
      ├── libs
   ├── vignette.qmd
   ├── vignette.html
├── README.md 
└── .gitignore
```

The data folder contains our raw, heart attack dataset and our preprocessed version of the dataset. The scripts folder contains a drafts folder, which is where group members can keep their work, and EDA.R and model-fitting.R, which are scripts with line annotations that replicate the results shown in the primary vignette document for the respective parts.

**The results folder contains vignette.qmd and vignette.html, which are the primary vignette documents.** The results folder also contains the folder vignette_files with the folder figure-html, which contains the plots in the vignette HTML.

## References

Our data: <https://www.kaggle.com/datasets/rashikrahmanpritom/heart-attack-analysis-prediction-dataset?select=heart.csv>

#### Learn about SVM!

-   <https://www.youtube.com/watch?v=_YPScrckx28>
-   <https://scikit-learn.org/stable/modules/svm.html>

#### Learn about XGBoost!

-   <https://www.youtube.com/watch?v=TyvYZ26alZs>
-   <https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/>
