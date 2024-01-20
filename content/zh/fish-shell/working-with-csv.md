---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么?
CSV是"逗号分隔值"的简称，是存储表格数据的一种文本格式，常用于导入和导出数据。程序员处理CSV文件可以快速交换数据，并与各种程序和在线服务协作。

## How to: 如何操作
```Fish Shell
# 读取CSV文件中的内容
cat example.csv

# example.csv的内容
# name,age,city
# Alice,30,New York
# Bob,22,Los Angeles

# 显示文件的第二行
sed -n '2p' example.csv
# 输出：Alice,30,New York

# 将CSV中的数据转换为以换行符分隔的条目
awk -F "," '{print $1 "\n" $2 "\n" $3}' example.csv
# 输出：
# name
# age
# city
# Alice
# 30
# New York
# Bob
# 22
# Los Angeles
```

## Deep Dive 深入探讨
CSV格式源自20世纪早期，用于简化复杂数据的表示。相较于Excel文件或数据库，CSV占用空间小，可读性强。Fish Shell不专门用于处理CSV，但可结合`awk`、`sed`等工具来操作。We also have CSV-specific command line tools like `csvkit` and `xsv` which offer more advanced functionalities. 

## See Also 更多信息
- Fish Shell官方文档：https://fishshell.com/docs/current/index.html
- GNU `awk`手册：https://www.gnu.org/software/gawk/manual/gawk.html
- `sed`简介：https://www.gnu.org/software/sed/manual/sed.html
- `csvkit`工具：https://csvkit.readthedocs.io/en/latest/
- `xsv`工具：https://github.com/BurntSushi/xsv