---
title:                "处理csv的工作"
html_title:           "Python: 处理csv的工作"
simple_title:         "处理csv的工作"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/python/working-with-csv.md"
---

{{< edit_this_page >}}

## 为什么

CSV（逗号分隔值）是一种常用的文件格式，用于存储和传输数据。它简单易懂，可以在不同的程序之间进行互操作，是处理数据的理想选择。在Python中，我们可以很容易地读取和写入CSV文件，并且通过使用一些库和技巧，还可以处理复杂的数据。

## 如何操作

要使用Python处理CSV文件，我们需要先导入csv库。接下来，我们可以使用`csv.reader()`函数来读取CSV文件中的数据，并将其存储为一个列表。

例如，假设我们有一个名为“students.csv”的文件，其中包含学生的姓名和分数。我们想要读取这个文件，并打印出每个学生的姓名和分数。

```Python
import csv

with open('students.csv', 'r') as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        print(row[0], "|", row[1])
```

这将输出类似以下内容的值：

```
John | 85
Emily | 92
Tom | 78
```

我们还可以使用`csv.writer()`函数来创建一个新的CSV文件，并向其中写入数据。例如，我们可以创建一个包含学生学号和平均成绩的新文件“grades.csv”。

```Python
import csv

with open('grades.csv', 'w') as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(['Student ID', 'Average Grade'])
    writer.writerow(['001', '89'])
    writer.writerow(['002', '76'])
    writer.writerow(['003', '95'])
    writer.writerow(['004', '81'])
```

这将创建一个包含以下内容的新文件：

```
Student ID | Average Grade
001 | 89
002 | 76
003 | 95
004 | 81
```

## 深入了解

除了基本的读取和写入操作，Python还提供了一些强大的库来处理CSV文件中的数据。例如，Pandas和Numpy库可以帮助我们对CSV数据进行统计分析和演算。

此外，Python还有一些特殊的模块，如csv.DictReader和csv.DictWriter，可以让我们更轻松地操作具有列标题的CSV文件。

## 查看也许会有所帮助

* [Python csv文档](https://docs.python.org/3/library/csv.html)
* [Pandas文档](https://pandas.pydata.org/pandas-docs/stable/reference/api/pandas.read_csv.html)
* [Numpy文档](https://numpy.org/doc/stable/reference/generated/numpy.genfromtxt.html)
* [Real Python教程：处理CSV文件](https://realpython.com/python-csv/)