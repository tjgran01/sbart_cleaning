import pandas as pd

class BalloonFeatureExtractor(object):
    '''
    Class that extracts relevant features from a participants raw trial file.
    i.e. extracts info about their individual session.

    Args:
        id(int): participant ID.
        fname = filepath to the .csv file to extract session data from.

    Returns:
        BalloonFeatureExtractor(instance)
    '''
    def __init__(self, id, fname):
        self.id = id
        self.fname = fname
        self.df = pd.read_csv(fname)


    def extract_balloon_features(self):

        balloon_data = []

        balloons = self.df.groupby(["BalloonCount"])
        for balloon in balloons:
            # Get start time and stop time.
            outcome = "cashed"
            start_row = balloon[1][balloon[1]["NewBalloon"] == 1.0]
            end_row = balloon[1][balloon[1]["CashIn"] == 1.0]
            # If they didn't cash in then they popped.
            if end_row.shape[0] == 0:
                end_row = balloon[1][balloon[1]["Pop"] == 1.0]
                outcome = "popped"

            if end_row.shape[0] == 0 or start_row.shape[0] == 0:
                continue
            else:
                # I have two weird balloons here.
                try:
                    total_balloon_time = float(end_row["Onset"]) - float(start_row["Onset"])
                except:
                    print(f"Error extracting rt data from participant: {self.id} balloon #: {balloon[0]}")
                    continue

            tokens = end_row["Tokens"].tolist()[0]
            pump_presses = sum(balloon[1]["PressPump"].tolist())
            pump_releases = sum(balloon[1]["ReleasePump"].tolist())

            balloon_data.append({"balloon_number": [balloon[0]],
                                 "outcome": [outcome],
                                 "total_balloon_time (s)": [total_balloon_time],
                                 "number_of_tokens": [tokens],
                                 "number_of_pump_actions": [pump_presses],
                                 "number_of_release_actions": [pump_releases]})

        return balloon_data
