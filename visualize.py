import csv
import matplotlib.pyplot as plt

# 读取CSV文件并统计Noeuds的数量
noeuds_counts = []
with open('nombre_noeuds.csv', 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    next(csvreader)  # 跳过头部
    for row in csvreader:
        noeuds_counts.append(int(row[0]))  # Noeuds的数量在第一列

# 绘制柱状图
plt.hist(noeuds_counts, bins=range(0, 200, 1), edgecolor='black')  # bins的范围和大小根据需要调整
plt.title('Distribution des Noeuds')
plt.xlabel('Nombre de Noeuds')
plt.ylabel('Fréquence')
plt.show()
