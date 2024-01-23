---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
处理CSV就是读写以逗号分隔的值文件。程序员常用它来交换数据，因为简单易懂，软件支持广泛。

## How to: (如何操作)
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp = fopen("example.csv", "w");

    // Assuming file opens successfully.
    fprintf(fp, "%s,%s,%s\n", "Name", "Age", "City");
    fprintf(fp, "%s,%d,%s\n", "Alice", 30, "Wonderland");
    fprintf(fp, "%s,%d,%s\n", "Bob", 25, "Bikini Bottom");
    
    fclose(fp);

    // Now reading the file.
    fp = fopen("example.csv", "r");
    char buffer[255];
    while (fgets(buffer, sizeof(buffer), fp)) {
        printf("%s", buffer);
    }

    fclose(fp);
    return 0;
}
```

Sample output:
```
Name,Age,City
Alice,30,Wonderland
Bob,25,Bikini Bottom
```

## Deep Dive (深入了解)
早期，CSV格式就被用于简单文本数据交换。现存其他格式如XML和JSON，但因其易用性，CSV仍受欢迎。在C中处理CSV需要手动解析文本数据，可能会涉及字符编码，转义，跳过标题或处理引号等复杂性。

## See Also (另见)
- RFC 4180, CSV标准: https://tools.ietf.org/html/rfc4180
- C标准库函数文档: https://en.cppreference.com/w/c/io
- CSV处理库Libcsv: http://sourceforge.net/projects/libcsv/
