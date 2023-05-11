import os

root_dir = 'D:'

for root, dirs, files in os.walk(root_dir):
    for folder in dirs:
        if folder.endswith("_lateral"):
            labeled_folder = os.path.join(root, folder)
            unlabeled_folder = os.path.join(root, folder[:-8])

            if os.path.exists(labeled_folder):
                for file in os.listdir(unlabeled_folder):
                    if file.endswith(".png"):
                        labeled_file = file[:-4] + '_labeled.png'
                        labeled_file_path = os.path.join(labeled_folder, labeled_file)

                        if not os.path.exists(labeled_file_path):
                            unlabeled_file_path = os.path.join(unlabeled_folder, file)
                            os.remove(unlabeled_file_path)