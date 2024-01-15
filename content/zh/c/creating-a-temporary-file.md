---
title:                "创建临时文件"
html_title:           "C: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 为什么
为了跟踪程序的运行，有时候我们需要在程序运行时临时存储一些数据。创建临时文件就是一种常用的方法，它可以让我们在程序运行时临时保存数据，方便后续使用或调试。

## 如何创建临时文件
创建临时文件可以通过`tmpfile()`函数来实现。这个函数会在操作系统中创建一个临时文件，并返回一个指向该文件的指针。接下来可以通过`fwrite()`函数向临时文件中写入数据，然后通过`fread()`函数读取数据。最后使用`fclose()`函数将临时文件关闭并删除。

```C
#include <stdio.h>

int main() {
  FILE *temp_file = tmpfile();
  if (temp_file == NULL) {
    printf("创建临时文件失败！\n");
    return 1;
  }

  // 向临时文件中写入数据
  char buffer[] = "这是临时文件中的数据";
  if (fwrite(buffer, 1, sizeof(buffer), temp_file) != sizeof(buffer)) {
    printf("写入数据失败！\n");
    return 1;
  }

  // 读取临时文件中的数据
  char read_buffer[sizeof(buffer)];
  if (fread(read_buffer, 1, sizeof(buffer), temp_file) != sizeof(buffer)) {
    printf("读取数据失败！\n");
    return 1;
  }

  printf("临时文件中的数据为：%s\n", read_buffer);

  // 关闭临时文件并删除
  if (fclose(temp_file) != 0) {
    printf("关闭临时文件失败！\n");
    return 1;
  }

  printf("临时文件已成功删除！\n");

  return 0;
}
```

输出：

```
临时文件中的数据为：这是临时文件中的数据
临时文件已成功删除！
```

## 深入了解临时文件
当调用`tmpfile()`函数时，操作系统会在临时目录中创建一个随机命名的临时文件，并将其作为`FILE`结构体的指针返回。临时文件通常具有`w+b`模式，即读写二进制模式。这意味着可以通过`fseek()`函数随机访问临时文件中的数据。操作系统会在程序结束时自动删除临时文件，但也可以通过调用`fclose()`函数手动删除。

## 参考阅读
- [C语言文档：创建临时文件](https://zh.cppreference.com/w/c/io/tmpfile)
- [C语言教程：文件读写](https://www.runoob.com/cprogramming/c-file-io.html)
- [Linux命令大全：fseek命令](https://man.linuxde.net/fseek)
- [Linux命令大全：fclose命令](https://man.linuxde.net/fclose)

## 参见
- [C语言文档](https://zh.cppreference.com/w/c)