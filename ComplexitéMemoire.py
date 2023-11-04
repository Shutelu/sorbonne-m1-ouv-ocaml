import csv
import matplotlib.pyplot as plt

# 读取CSV文件并统计Noeuds的数量
noeuds_apres_counts = []
with open('nombre_noeuds_apres.csv', 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    for row in csvreader:
        noeuds_apres_counts.append(int(row[0]))  # Noeuds的数量在第一列

# 绘制折线图
plt.plot(range(1, len(noeuds_apres_counts) + 1), noeuds_apres_counts, marker='', linestyle='-', color='b')
plt.title('Nombre de Noeuds Apres Compression')
plt.xlabel('Nombre de Bits')
plt.ylabel('Nombre de Noeuds')
plt.savefig('noeuds_apres_compression.png')  # 保存图像到本地
plt.show()
