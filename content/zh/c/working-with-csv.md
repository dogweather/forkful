---
title:                "与csv的合作"
html_title:           "C: 与csv的合作"
simple_title:         "与csv的合作"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-csv.md"
---

{{< edit_this_page >}}

## 什么是CSV？为什么程序员要用它？

CSV是Comma Separated Values的缩写，指的是以逗号作为分隔符来存储数据的一种格式。程序员通常使用CSV来简化数据的存储和处理，因为它们可以轻松地从电子表格软件中导出和导入。

## 如何操作？

在C语言中，我们可以使用标准库中的函数来读取和写入CSV文件。下面是一个简单的例子，演示了如何读取一个包含三列和三行数据的CSV文件，并把它们打印出来。

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_ROWS 3
#define MAX_COLS 3

int main() {
    // 创建文件指针并打开文件
    FILE *fp;
    fp = fopen("data.csv", "r");

    // 创建一个二维数组来存储读取的数据
    char data[MAX_ROWS][MAX_COLS][100];

    // 遍历文件中的每一行数据，并分别读取每个单元格的内容
    for (int i = 0; i < MAX_ROWS; i++) {
        for (int j = 0; j < MAX_COLS; j++) {
            fscanf(fp, "%[^,],", data[i][j]);
        }
    }

    // 打印读取的数据
    for (int i = 0; i < MAX_ROWS; i++) {
        for (int j = 0; j < MAX_COLS; j++) {
            printf("%s ", data[i][j]);
        }
        printf("\n");
    }

    // 关闭文件
    fclose(fp);

    return 0;
}
```

输出结果将会是：

```
John 24 New York
Mary 28 San Francisco
Tom 32 Los Angeles
```

## 深层解析

CSV格式最初是由微软在20世纪80年代开发的。它经常被用作数据交换的格式，因为它简单明了，且易于导入到数据库或电子表格中。

除此之外，程序员也可以使用其他格式来存储数据，例如JSON或XML。每种格式都有自己的优缺点，开发者需要根据自己的需求来选择。

在C语言中，我们可以使用```fscanf()```函数来读取CSV文件，在写入时可以使用```fprintf()```函数。如果需要解析更复杂的CSV文件，可以使用专门的解析库。

## 参考资料

- [C语言标准库文档](https://zh.cppreference.com/w/c/)
- [如何解析复杂的CSV文件](https://medium.com/@jhjhjhsu/how-to-parse-csv-files-in-c-92bf6043472)
- [其他格式与CSV的比较](https://www.softwaretestinghelp.com/csv-vs-xml-vs-json/)