# Python 脚本
import pandas as pd
import matplotlib.pyplot as plt

# 读取 CSV 文件
df = pd.read_csv('nombre_noeuds.csv')

# 创建折线图
plt.figure()
plt.plot(df['Index'], df['NombreDeNoeuds'], marker='o')

# 设置图表标题和轴标签
plt.title('Nombre de Noeuds dans l\'Arbre')
plt.xlabel('Index')
plt.ylabel('Nombre de Noeuds')

# 显示网格
plt.grid(True)

# 保存图表为图片文件
plt.savefig('nombre_noeuds_graph.png')

# 显示图表
plt.show()
