import os

class DataSetCollater(object):
    '''
    Class which runs through all the data files and matches IDs to
    their corresponding files.

    Args:
        root_dir(string): the root directory where all the data is stored.

    Returns:
        DataSetCollator(instance)
    '''
    def __init__(self, root_dir=f"{os.getcwd()}/../../BehData/"):
        self.root_dir = root_dir
        self.data_dict = self.create_data_dict()


    def create_data_dict(self):
        '''
        Walks through the files in the behavioral directory and
        extracts the main behavioral data file.

        Args:
            None (uses self.root_dir).

        Returns:
            id_fpath_dict(int, string [fpath]): a dictionary with participant IDs
            as keys and a filepath to their bart data as a value.
        '''

        id_fpath_dict = {  }
        for root, dirs, files in os.walk(self.root_dir):
            for fname in files:
                if fname.startswith("new_D"):
                    id_fpath_dict[int(fname.split("_")[-1][:-4])]= os.path.join(root, fname)
        return id_fpath_dict


    def print_missing(self):
        '''
        Displays the (potential) missing files for IDs.

        Args:
            None (uses self.data_dict).

        Returns:
            None (outputs).
        '''

        accounted_for = self.data_dict.keys()
        potentials = [x for x in range(max(accounted_for))]
        missing = [x for x in potentials if x not in accounted_for]
        print(missing)




if __name__ == "__main__":
    collater = DataSetCollater()
