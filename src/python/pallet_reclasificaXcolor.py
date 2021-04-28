import cv2
import argparse
import os
import csv
import numpy as np

def load_csv(csv_file):
    with open(csv_file, newline='') as csvfile:
        csvread = csv.reader(csvfile, delimiter=',')
        result={}
        for row in csvread: # For every line (i.e for every bounding box) in the CSV file...
            jpg=row[0].strip()
            if result.get(jpg,None) is None:
                result[jpg]=[ [int(row[1]),int(row[2]),int(row[3]),int(row[4]),int(row[5])] ]
            else:
                result[jpg].append([int(row[1]),int(row[2]),int(row[3]),int(row[4]),int(row[5])])
        return result

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--in-dir', required=True, help='Directorio del DS actual')
    parser.add_argument('--out-dir', required=True, help='Directorio de salida con DS modificado')    
    args = parser.parse_args()
    os.makedirs(os.path.join(args.out_dir,"annotations"),exist_ok=True)
    train_labels_file="labels-train.csv"
    val_labels_file="labels-val.csv"
    labels=[train_labels_file,val_labels_file]
    for label_file in labels:
        d_labels=load_csv(os.path.join(args.in_dir,label_file))
        labels_out={}
        for jpg_name,bboxes in d_labels.items():
            print("{}\t\t{}".format(jpg_name,bboxes))
            img=cv2.imread(os.path.join(args.in_dir,jpg_name))
            cv2.imwrite(os.path.join(args.out_dir,jpg_name),img)
            for bbox in bboxes:
                if labels_out.get(jpg_name,None) is None:
                    labels_out[jpg_name]=[]
                if bbox[4]!=0:
                    xmin,xmax,ymin,ymax,clazz=bbox
                    clip = img[ymin:ymax,xmin:xmax,:]
                    avg_color=np.mean(clip)
                    clazz=1 if avg_color>80 else 2
                    bbox[4]=clazz
                labels_out[jpg_name].append(bbox)
        with open(os.path.join(args.out_dir,label_file),"wt") as out_csv:
            for jpg_name,bboxes in labels_out.items():
                for bbox in bboxes:
                    xmin,xmax,ymin,ymax,clazz=bbox
                    out_csv.write("{},{},{},{},{},{}\n".format(jpg_name,xmin,xmax,ymin,ymax,clazz))

            

                    



