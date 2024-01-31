---
title:                "处理 CSV 文件"
date:                  2024-01-19
simple_title:         "处理 CSV 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? - 什么是CSV，以及为什么使用它？
CSV是一种简单的文件格式，用来存储表格数据，如数字和文字。程序员用它因为它简单，通用，很容易被不同程序所读取和写入。

## How to - 如何操作：
```Java
import java.io.*;
import java.util.*;

public class CSVExample {

    public static void main(String[] args) throws IOException {
        // 写CSV
        FileWriter csvWriter = new FileWriter("example.csv");
        csvWriter.append("姓名").append(",").append("年龄").append("\n");
        csvWriter.append("李雷").append(",").append("30").append("\n");
        csvWriter.append("韩梅梅").append(",").append("28").append("\n");
        csvWriter.flush();
        csvWriter.close();

        // 读CSV
        BufferedReader csvReader = new BufferedReader(new FileReader("example.csv"));
        String row;
        while ((row = csvReader.readLine()) != null) {
            String[] data = row.split(",");
            // 打印数据
            System.out.println("姓名: " + data[0] + ", 年龄: " + data[1]);
        }
        csvReader.close();
    }
}
```
输出样例：
```
姓名: 李雷, 年龄: 30
姓名: 韩梅梅, 年龄: 28
```

## Deep Dive - 更深入的信息：
1. 历史背景：CSV的格式起源很早，上世纪70年代就开始使用了。
2. 替代品：除了CSV，还有如JSON、XML等更复杂的格式用于存储和传输结构化数据。
3. 实现细节：标准CSV没有明确标准，但是一般遵循一行一条记录，以逗号分隔值。注意，有时候数据可以包含逗号，这时候通常会用双引号将这些数据括起来。

## See Also - 相关资源：
- Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- OpenCSV: http://opencsv.sourceforge.net/
- Java官方文档: https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html

阅读完本文后，不妨亲自动手实践或探索更多的库来处理CSV文件！
