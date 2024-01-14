---
title:                "C: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 为什么
如果您是一个初学者或者是一个有经验的程序员，您可能会想要开始一个新的项目。这可能是因为您想学习新的编程技能，或者是因为您想要探索一个新的主题。无论是什么原因，开始一个新的项目都是锻炼自己技能和提高自己的能力的好方法。

## 如何
下面我将用一些示例来介绍如何开始一个新的C编程项目。首先，我们需要创建一个新的C程序文件。打开您喜欢的文本编辑器，例如Notepad++或者Sublime Text。然后在新文件中输入以下代码：

```C
#include <stdio.h>

int main()
{
   printf("欢迎来到我的C编程项目！\n");
   return 0;
}
```

保存这个文件，并将它命名为“hello.c”。然后打开命令行界面，进入保存文件的目录，并输入以下命令来编译程序：

```
gcc hello.c -o hello
```

这里我们使用gcc编译器来编译程序，并将编译后的可执行文件命名为“hello”。最后，我们可以运行程序：

```
./hello
```

您将会看到屏幕上输出了“欢迎来到我的C编程项目！”。恭喜，您已经成功开始了一个新的C编程项目！

## 深入探讨
在开始一个新的C编程项目之前，您可能想要考虑一些问题。首先，您想要解决哪个问题？确定您的项目的目的和目标非常重要。其次，您将需要哪些工具？选择合适的文本编辑器和编译器是成功的关键。最后，您是否需要学习新的C语法或使用新的库？确保您有充分的准备和知识来完成您的项目是非常重要的。

## 参考
了解C语言：https://www.learn-c.org/
使用Sublime Text编写C代码：https://www.freecodecamp.org/news/building-and-running-c-programs-with-sublime-text-c031a59e9756/
常用的C库：https://www.geeksforgeeks.org/top-10-libraries-in-c/
命令行界面入门指南：https://www.codecademy.com/articles/command-line-setup