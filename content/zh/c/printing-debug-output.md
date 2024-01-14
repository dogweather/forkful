---
title:    "C: 打印调试输出"
keywords: ["C"]
---

{{< edit_this_page >}}

#为什么

当我们在编写程序时，总会遇到各种各样的错误。有时候，这些错误会让我们感觉束手无策，不知道程序是在哪里出错了。这时候，打印调试信息就非常有用。通过打印调试信息，我们可以知道程序具体的执行过程，从而更容易找到问题所在。

#如何打印调试信息

在C语言中，打印调试信息非常简单。我们可以使用<stdio.h>头文件中的printf函数来打印信息。在下面的例子中，我们使用printf函数打印了一个数字和一个字符串。

```C
#include <stdio.h>
int main() {
  int num = 2020;
  char str[] = "欢迎来到我的博客！";
  printf("数字：%d \n字符串：%s", num, str);
  return 0;
}
```
输出结果：
```
数字：2020 
字符串：欢迎来到我的博客！
```

#深入了解打印调试信息

除了使用printf函数，我们还可以使用其他一些调试技术来打印信息。比如，我们可以使用调试器来单步执行程序并查看变量的值。我们也可以使用宏来简化打印调试信息的过程。总的来说，打印调试信息是一个非常灵活和强大的调试工具，可以帮助我们更快地排除错误。

#另请参阅

- [C语言调试技巧](https://www.runoob.com/w3cnote/c-debug-tips.html)
- [使用调试器调试C程序](https://www.ibm.com/developerworks/cn/linux/content/opensource-cn-dbgcc/index.html)
- [C语言宏的使用及示例](https://www.runoob.com/w3cnote/c-macro-example.html)