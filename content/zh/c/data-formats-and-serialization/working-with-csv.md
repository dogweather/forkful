---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:30.914571-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728C\u8BED\u8A00\u4E2D\u8BFB\
  \u53D6CSV\u6587\u4EF6\uFF0C\u6211\u4EEC\u4F7F\u7528\u6807\u51C6\u6587\u4EF6I/O\u51FD\
  \u6570\u4EE5\u53CA\u5B57\u7B26\u4E32\u5904\u7406\u51FD\u6570\u6765\u89E3\u6790\u6BCF\
  \u4E00\u884C\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF0C\u5C55\
  \u793A\u4E86\u5982\u4F55\u8BFB\u53D6CSV\u6587\u4EF6\u5E76\u5C06\u6BCF\u884C\u7684\
  \u5B57\u6BB5\u6253\u5370\u5230\u63A7\u5236\u53F0\u3002"
lastmod: '2024-04-05T22:38:47.486021-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5728C\u8BED\u8A00\u4E2D\u8BFB\u53D6\
  CSV\u6587\u4EF6\uFF0C\u6211\u4EEC\u4F7F\u7528\u6807\u51C6\u6587\u4EF6I/O\u51FD\u6570\
  \u4EE5\u53CA\u5B57\u7B26\u4E32\u5904\u7406\u51FD\u6570\u6765\u89E3\u6790\u6BCF\u4E00\
  \u884C\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u57FA\u672C\u793A\u4F8B\uFF0C\u5C55\u793A\
  \u4E86\u5982\u4F55\u8BFB\u53D6CSV\u6587\u4EF6\u5E76\u5C06\u6BCF\u884C\u7684\u5B57\
  \u6BB5\u6253\u5370\u5230\u63A7\u5236\u53F0\u3002"
title: "\u5904\u7406CSV\u7684\u5DE5\u4F5C"
weight: 37
---

## 如何操作：


### 读取CSV文件
要在C语言中读取CSV文件，我们使用标准文件I/O函数以及字符串处理函数来解析每一行。下面是一个基本示例，展示了如何读取CSV文件并将每行的字段打印到控制台。

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
示例 `data.csv`内容：
```
Name,Age,Occupation
John Doe,29,Software Engineer
```

示例输出：
```
Name
Age
Occupation
John Doe
29
Software Engineer
```

### 写入CSV文件
同样地，写入一个CSV文件涉及使用`fprintf`以逗号分隔的格式保存数据。

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char *headers[] = {"Name", "Age", "Occupation", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Data Scientist");

    fclose(fp);
    return 0;
}
```

示例 `output.csv`内容：
```
Name,Age,Occupation
Jane Doe,27,Data Scientist
```

## 深入探讨
尽管CSV格式看似简单直接，但它有其细微的差别，比如处理字段内的逗号以及用引号封装字段。上面展示的基本示例没有考虑到这些复杂性，也没有做到错误处理。

历史上，由于C语言的低级特性和缺乏内置的高级抽象来处理此类任务，对CSV的处理大多数是手工进行的。这包括打开文件、读取行、分割字符串以及根据需要转换数据类型等手动管理。

虽然在C语言中直接操纵CSV文件为文件I/O和字符串处理提供了宝贵的学习经验，但一些现代的替代方法承诺高效和较少的错误发生。如`libcsv`和`csv-parser`这样的库提供了全面的读写CSV文件的函数，包括对引号字段和自定义分隔符的支持。

另外，当在支持它的生态系统中工作时，与提供高级CSV操作函数的语言或平台集成（比如使用其`pandas`库的Python）可能是针对需要大量CSV处理的应用程序更为生产的途径。这种跨语言的方法利用了C的性能和系统编程能力，同时利用其他语言在处理特定任务例如CSV处理方面的易用性。
