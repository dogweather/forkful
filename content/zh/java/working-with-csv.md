---
title:                "CSV操作指南"
html_title:           "Java: CSV操作指南"
simple_title:         "CSV操作指南"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/working-with-csv.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
CSV(Comma-Separated Values)是一种常用的文件格式，它可以方便地存储和传输大量数据。因为它的简单性和通用性，程序员经常使用CSV来处理和分析数据。

# 怎么做：
以下是使用Java处理CSV文件的示例代码和输出：
```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class CSVReader {

    public static void main(String[] args) {
        // 创建一个指向CSV文件的File对象
        File csvFile = new File("data.csv");
        
        // 创建一个Scanner对象来读取文件
        try {
            Scanner scanner = new Scanner(csvFile);

            // 设置分隔符为逗号
            scanner.useDelimiter(",");

            // 读取文件的每一行，直到文件结尾
            while (scanner.hasNextLine()) {

                // 读取一行数据并分割为若干个字段
                String[] fields = scanner.nextLine().split(",");

                // 打印每个字段
                for (String field : fields) {
                    System.out.println(field);
                }

                // 输出空行
                System.out.println();
            }

            // 关闭Scanner对象
            scanner.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }
}
```
输出：
```
John Smith
25
Male

Jane Doe
30
Female

Bob Johnson
40
Male
```

# 深入了解：
CSV文件格式已经存在很长时间，它最早是由Lotus软件开发用于表格数据的存储。现在，CSV已经成为了处理数据的标准方式，它的简单性使得它比其他复杂的数据格式更容易使用。虽然在处理大型数据时，JSON和XML等格式可能更合适，但在许多情况下，CSV仍然是最方便和最有效的选择。在Java中，可以使用第三方库如OpenCSV来更方便地处理CSV文件，也可以根据自己的需求编写自定义的CSV解析器。

# 参考资料：
- [Java官方文档](https://docs.oracle.com/javase/8/docs/api/java/io/)
- [OpenCSV官方网站](http://opencsv.sourceforge.net/)
- [CSV格式介绍](https://www.techopedia.com/definition/27724/csv-file)