---
title:                "读取文本文件"
html_title:           "Kotlin: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？
读取文本文件意味着你的程序可以处理在文件系统中储存的数据。编程人员这样做是因为它使得程序能以一种通用和便捷的方式获取和处理信息。

## 如何做：
以下是一段C语言读取文本文件的简单示例：

```C
#include<stdio.h>

int main(){
   char ch;
   FILE *fp;
   fp = fopen("test.txt", "r"); // 打开一个名为 "test.txt" 的文件

   if (fp == NULL){
      perror("打开文件时出错"); // 打开文件失败
      return(-1);
   }

   while((ch = fgetc(fp)) != EOF)  // 检查文件是否结束
      printf("%c",ch);

   fclose(fp);   // 关闭文件
   return 0;
}
```
当你运行上面程序时，你将从"test.txt"文件获取到的文本在控制台上显示。

## 深度探索：
读取文件的编程概念始于早期的电子计算机系统，这些早期系统经常依赖于磁带或打孔卡片来存储和	retrieve数据。

C编程语言提供了许多用于文件读写的函数，如 fopen，fclose，fscanf，fprintf，fgetc 和 fputc等。C提供这些函数是为了让程序员能够更加容易地从文件系统读取和写入数据。

虽然这份指南解释了如何在C语言中读取文本文件，但也存在其他方法。例如，你可以使用诸如 fread 和 fgets 的高级函数来读取更大块的数据。另外，也可能会用到其他语言来读取文件，如Python，Java或C++。

## 另请参阅:
文件io函数详情: https://www.runoob.com/cprogramming/c-file-io.html
更多文件读写样例: https://www.geekhideout.com/cfileio.shtml