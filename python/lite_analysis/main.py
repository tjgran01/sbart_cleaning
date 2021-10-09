import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

def main():

    FPATH = "../../test/sbart_balloon_features.csv"

    df = pd.read_csv(FPATH)
    df = df[df["total_balloon_time (s)"] < 100]
    ax = sns.histplot(df["total_balloon_time (s)"])
    ax = sns.displot(df["total"])
    plt.show()


if __name__ == "__main__":
    main()
