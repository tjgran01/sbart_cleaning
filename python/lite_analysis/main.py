import pandas as pd
import matplotlib.pyplot as plt
import statsmodels.api as sm
import seaborn as sns

def main():

    FPATH = "../../test/sbart_balloon_features.csv"

    df = pd.read_csv(FPATH)
    df = df[df["total_balloon_time"] < 50]

    model = sm.OLS(df["total_balloon_time"], df["number_of_tokens"]).fit()
    predictions = model.predict(df["total_balloon_time"])

    sns.histplot(predictions)
    print(model.summary())

    # ax = sns.histplot(df["hold_ratio"])
    plt.show()


if __name__ == "__main__":
    main()
