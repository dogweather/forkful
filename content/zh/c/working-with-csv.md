---
title:                "C: 使用CSV进行编程"
simple_title:         "使用CSV进行编程"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-csv.md"
---

{{< edit_this_page >}}

# 为什么

CSV是一种普遍使用的文件格式，它广泛应用于数据存储和交换中。如果您正在处理大量的数据集合，使用CSV文件格式可以让您更轻松地对数据进行操作和管理。

# 如何操作CSV文件

如果您使用C语言进行编程，那么在处理CSV文件时可以很方便地使用标准库中的`csv.h`头文件。下面是一个简单的代码示例：

```
#include <stdio.h>
#include "csv.h"

int main() {
    // 打开CSV文件
    FILE *fp = fopen("data.csv", "rb");
    
    //读取文件中的数据并存储在一个csv_parser结构中
    csv_parser *parser = csv_parser_new(fp);
    
    // 从CSV文件中逐行读取每条数据
    while (csv_read_row(parser)) {
        const char **row = csv_get_row(parser); //获得当前行的数据
        
        //输出每个数据字段的值
        for (int i = 0; i < csv_num_fields(row); i++) {
            printf("%s ", row[i]);
        }
        printf("\n");
    }
    
    // 关闭文件
    fclose(fp);
    
    // 释放parser内存
    csv_parser_free(parser);
    
    return 0;
}
```

上面的示例代码将打开一个名为"data.csv"的CSV文件，并逐行读取其中的数据，然后将每条数据按空格分隔输出。下面是示例代码的输出结果：

> 2019/01/01 ProductA 1000.00  
> 2019/01/02 ProductB 500.00  
> 2019/01/03 ProductC 800.00  

通过这种方式，您可以很轻松地读取CSV文件中的数据，并在程序中进行处理。

# 深入学习CSV

如果您想要更深入地学习如何处理CSV文件，可以进一步了解`csv.h`头文件中提供的各种函数和结构。例如，您可以使用`csv_set_comment_char()`函数设置CSV文件中的注释字符，或者使用`csv_read_record()`函数来读取指定行的数据。

此外，还建议您了解CSV文件的常用格式，例如每条数据都由逗号或制表符分隔，每条数据都以换行符结尾等。

# 请参考

- [csv.h头文件文档](https://www.poftut.com/csv-values-handling-in-c-open-source-csv-c-library/)
- [关于CSV文件格式的简介](https://www.sohamkamani.com/blog/2016/10/14/parsing-csvs-in-c/)
- [使用C语言处理CSV文件的实践](https://codereview.stackexchange.com/questions/216700/parsing-a-simple-csv-format-in-c)