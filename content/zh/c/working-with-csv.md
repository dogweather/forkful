---
title:                "与CSV的工作"
html_title:           "C: 与CSV的工作"
simple_title:         "与CSV的工作"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/working-with-csv.md"
---

{{< edit_this_page >}}

# 为什么

CSV文件是一个常见的数据交换格式，它允许我们轻松地在不同的程序和系统之间共享数据。使用C语言处理CSV文件可以帮助我们更轻松地处理和分析数据，并将其应用于我们的实际项目中。

# 如何做

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
  // 打开要处理的CSV文件
  FILE *file = fopen("data.csv", "r");
  
  // 检查文件是否打开成功
  if (file == NULL) {
    printf("无法打开CSV文件。\n");
    return -1;
  }

  // 创建一个二维数组来存储CSV文件中的数据
  double data[10][3];
  
  // 使用循环来读取CSV文件中的数据，并存储到数组中
  for (int i = 0; i < 10; i++) {
    fscanf(file, "%lf,%lf,%lf", &data[i][0], &data[i][1], &data[i][2]);
  }
  
  // 关闭文件
  fclose(file);

  // 打印数组中的数据来验证是否读取成功
  for (int i = 0; i < 10; i++) {
    printf("%.2f  %.2f  %.2f\n", data[i][0], data[i][1], data[i][2]);
  }

  return 0;
}
```

这段代码中，我们首先使用`fopen()`函数打开了一个名为`data.csv`的CSV文件，并指定了文件模式为“读取”（`r`）。然后，我们使用`fscanf()`函数来读取每一行数据，并将其存储到二维数组中。最后，我们使用`fclose()`函数关闭文件。通过循环打印数组中的数据，我们可以看到CSV文件中的内容已经成功地读取并存储到了数组中。

# 深入了解

要处理复杂的CSV文件，我们还可以使用C语言中的一些强大的库函数，比如`strtok()`来分割字符串和`atoi()`来将字符串转换为整数。另外，我们还可以使用`fprintf()`函数来将数据写入CSV文件，以及使用`fgets()`函数来读取一整行数据。在实际项目中，我们可以结合这些函数来更灵活地处理CSV文件，并根据具体需求进行操作。

# 参考文献

- [C语言处理CSV文件的方法和技巧](https://blog.csdn.net/zsk875/article/details/93506377)
- [C语言文本文件操作：fopen、fclose、fgets、fputs](https://www.cnblogs.com/ggjucheng/archive/2010/06/10/1755320.html)
- [C语言处理字符串的方法大全](https://www.cnblogs.com/liangyue/p/8297078.html)

# 参见

[CSV文件格式](https://zh.wikipedia.org/zh-hans/CSV)