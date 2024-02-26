"""Module providing a functionality to imputed missing values from dataframe."""

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from missingpy import MissForest
from sklearn.impute import IterativeImputer, KNNImputer
from sklearn.linear_model import BayesianRidge


class DataImputation:
    """
    Initializes a DataImputation object.

    Args:
        data (pd.DataFrame): The dataframe containing the data.
        cols (list): The list of columns to perform imputation on.

    Info From:
        https://www.youtube.com/watch?v=7DQfFbfZF3o
    """

    def __init__(self, data: pd.DataFrame, cols: list):
        self.data = data
        self.cols = cols

    def simple_imputation(self, method="media"):
        if method == "media":
            for col in self.cols:
                self.data[f"{col}_imputed_media"] = self.data[col].fillna(
                    self.data[col].mean()
                )
        elif method == "mediana":
            for col in self.cols:
                self.data[f"{col}_imputed_mediana"] = self.data[col].fillna(
                    self.data[col].median()
                )
        elif method == "mode":
            for col in self.cols:
                self.data[f"{col}_imputed_mode"] = self.data[col].fillna(
                    self.data[col].mode()[0]
                )
        elif method == "KNN":
            imputer = KNNImputer(n_neighbors=3)
            for col in self.cols:
                self.data[f"{col}_imputed_knn"] = imputer.fit_transform(self.data[col])
        elif method == "randforest":
            imputer = MissForest(max_iter=2)
            for col in self.cols:
                self.data[f"{col}_imputed_knn"] = imputer.fit_transform(self.data[col])
        else:
            methods = ["media", "mediana", "mode", "KNN", "randforest"]
            print(f"Para la imputación simple debe ingresar: {methods}")

    def multiple_imputation(self, method="MICE"):
        if method == "MICE":
            imputer = IterativeImputer(random_state=0, estimator=BayesianRidge())
            for col in self.cols:
                self.data[f"{col}_imputed_MICE"] = imputer.fit_transform(self.data[col])

    def compare_imputation(self, col1, col2):
        plt.figure(figsize=(10, 6))
        sns.kdeplot(data=self.data, x=col1, label=col1, fill=True)
        sns.kdeplot(data=self.data, x=col2, label=col2, fill=True)

        plt.title("Gráfico de Densidad")
        plt.xlabel("Value")
        plt.ylabel("Density")
        plt.legend()
        plt.show()
