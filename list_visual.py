import csv
import matplotlib.pyplot as plt

# 读取CSV文件
taux_compression = []
with open('taux_compression.csv', 'r') as csvfile:
    csvreader = csv.reader(csvfile)
    next(csvreader)  # 跳过头部
    for row in csvreader:
        taux_compression.append(float(row[1]))  # 压缩率在第二列

# 绘制折线图
plt.plot(taux_compression)  # 移除了 marker 参数
plt.title('Taux de Compression')
plt.xlabel('Index')
plt.ylabel('Taux de Compression')
plt.grid(True)  # 显示网格
plt.savefig('taux_compression.png')  # 保存图像到文件
plt.show()  # 显示图像
