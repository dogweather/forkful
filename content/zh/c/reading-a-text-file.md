---
title:                "读取文本文件"
html_title:           "C: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

写程序是一件有趣的事情，尤其是当你能够创建一些能够处理复杂任务的程序时。读取文本文件是一项非常基本的任务，但却是编程中的一个重要部分。本文将向你展示如何使用C语言来读取文本文件。

## 如何进行

下面是一个简单的C语言程序示例，展示了如何读取一个文本文件并将其打印到屏幕上：

```C
#include <stdio.h>

int main(){
    // 打开文件
    FILE *file = fopen("example.txt", "r");
    
    // 检查文件是否成功打开
    if (file == NULL){
        printf("无法打开文件！");
        return 1;
    }
    
    // 读取文件内容并打印到屏幕上
    char c;
    while ((c = fgetc(file)) != EOF){
        printf("%c", c);
    }
    
    // 关闭文件
    fclose(file);
    
    return 0;
}
```

假设我们有一个名为"example.txt"的文本文件，其中的内容为：

```
这是一段文本。
它有多行。
```

运行上面的程序，我们将得到以下输出：

```
这是一段文本。
它有多行。
```

现在，让我们来解释一下这段代码的工作原理。

首先，我们需要包含<stdio.h>头文件，这样我们才能使用C语言提供的文件操作函数。然后，我们使用fopen()函数来打开一个文件。它的第一个参数是文件的名称，第二个参数是文件的打开模式。在这个例子中，我们使用了"r"模式来表示我们是要读取文件。如果文件成功打开，fopen()函数将返回一个指向该文件的指针；否则，它将返回NULL。

接下来，我们使用一个if语句来检查文件是否成功打开。如果没有成功，我们将打印一条错误信息并退出程序。否则，我们将使用fgetc()函数来逐个读取文件中的字符，直到到达文件结尾（由EOF表示）。每次读取一个字符后，我们使用printf()函数将它打印到屏幕上。

最后，我们使用fclose()函数来关闭文件，以防止出现任何错误。

## 深入了解

除了上面我们提到的fopen()、fgetc()和fclose()函数之外，C语言还提供了许多其他函数来读取文本文件。例如，你可以使用fgets()函数来一次读取一行文本，或者使用fscanf()函数来按照指定的格式来读取文件内容。此外，你还可以使用fseek()函数来定位到文件的特定位置，或者使用ftell()函数来获取文件指针的当前位置。

如果你对C语言的文件操作函数感兴趣，可以参考下面的链接来了解更多信息：

- [C语言文件操作简介](https://www.runoob.com/cprogramming/c-file-handling.html)
- [C语言文件读写教程](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

## 参考

- [C语言中文网 - 文件操作](http://c.biancheng.net/view/31.html)
- [C语言中文网 - 文件操作函数列表](http://c.biancheng.net/cpp/biancheng/view/3018.html)