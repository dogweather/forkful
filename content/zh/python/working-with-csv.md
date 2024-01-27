---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
为何及为何？

CSV文件存储表格数据，像Excel表。程序员用CSV因为简单，兼容性强，易于人读写。

## How to:
如何操作：

```Python
import csv

# 写CSV
with open('example.csv', 'w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    # 写入标题行
    writer.writerow(["姓名", "年龄", "城市"])
    # 写入数据行
    writer.writerow(["小明", "25", "上海"])
    writer.writerow(["小红", "30", "北京"])

# 读CSV
with open('example.csv', 'r', encoding='utf-8') as file:
    reader = csv.reader(file)
    for row in reader:
        print(row)

# 输出:
# ['姓名', '年龄', '城市']
# ['小明', '25', '上海']
# ['小红', '30', '北京']
```

## Deep Dive
深度探究：

1. CSV历史久，格式简，早期用于数据交换。
2. 替代方案：JSON, XML，但CSV数据量大时更高效。
3. CSV模块支持多种格式，可定制分隔符等。

## See Also
参见链接：

- 官方CSV模块文档：https://docs.python.org/3/library/csv.html
- pandas处理大型CSV：https://pandas.pydata.org/
- 开放数据集CSV样本：https://www.kaggle.com/datasets
