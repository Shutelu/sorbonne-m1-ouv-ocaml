import csv
import matplotlib.pyplot as plt

# 读取CSV文件并统计Noeuds的数量
noeuds_counts = []
with open('nombre_noeuds.csv', 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    next(csvreader)  # 跳过头部
    for row in csvreader:
        noeuds_counts.append(int(row[0]))  # 假设Noeuds的数量在第一列

# 绘制柱状图
plt.hist(noeuds_counts, bins=range(0, 1001, 1), edgecolor='black')  # bins的范围和大小根据需要调整
plt.title('Graphe de Noeuds dans ZDD')
plt.xlabel('Nombre de Noeuds')
plt.ylabel('Probabilité(%) d\'apparition ')

# 保存图像到本地文件
plt.savefig('distribution_noeuds.png', format='png')

# 显示图像
plt.show()
