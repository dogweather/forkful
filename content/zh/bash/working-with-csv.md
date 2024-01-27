---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么?)
CSV（逗号分隔值）格式是保存表格数据的一种简单文件格式，常用于交换不同软件之间的数据。编程时处理CSV文件，可方便地导入导出数据，实现数据的快捷分析和处理。

## How to (如何操作):

```Bash
# Reading a CSV file line by line
while IFS=, read -r column1 column2 column3
do
  echo "Column 1: $column1 - Column 2: $column2 - Column 3: $column3"
done < example.csv

# Output:
# Column 1: value1 - Column 2: value2 - Column 3: value3
# ... (additional lines from the CSV file)

# Writing to a CSV file
echo "new1,new2,new3" >> example.csv
```

## Deep Dive (深入了解):
CSV格式起源于早期计算机，因其格式简单、兼容性强，一直被广泛使用。尽管有XML和JSON等现代替代格式，CSV仍因其简洁性和广泛支持而保持重要地位。处理CSV文件时，Bash脚本可以通过内置文本处理命令如`cut`, `sort`, `awk`实现复杂操作，但不适合处理具有嵌套引号或逗号的复杂CSV数据。

## See Also (另请参阅):
- [GNU Coreutils](https://www.gnu.org/software/coreutils/)
- [Bash Reference Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Awesome Shell](https://github.com/alebcay/awesome-shell)
- [Introduction to `awk`](https://www.gnu.org/software/gawk/manual/gawk.html)
