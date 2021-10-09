from Collater import DataSetCollater
from balloon_features import BalloonFeatureExtractor
import pandas as pd

def main():

    EXPORT_DIR = "../../test/"
    collater = DataSetCollater()

    frames = []
    for id, fname in collater.data_dict.items():
        extractor = BalloonFeatureExtractor(id, fname)
        features = extractor.extract_balloon_features()
        df = pd.concat([pd.DataFrame.from_dict(balloon) for balloon in features])
        df["id"] = [id for x in range(df.shape[0])]
        frames.append(df)


    all = pd.concat(frames, ignore_index=True)
    all.to_csv(f"{EXPORT_DIR}sbart_balloon_features.csv", index=False)







if __name__ == "__main__":
    main()
