---
title:                "将字符串转换为小写"
html_title:           "C: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，经常会遇到需要将字符串转换为小写字母的情况。这可能是因为用户输入的大小写不一致，或者需要对字符串进行比较时，忽略大小写的影响。因此，了解如何将字符串转换为小写是非常重要的。

## 如何操作

在C语言中，有一个内置函数`tolower()`可以实现将字符串转换为小写。下面是一个示例代码：

```
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "HELLO WORLD";
  int i = 0;

  while (str[i]) {
    putchar(tolower(str[i]));
    i++;
  }

  printf("\n");

  return 0;
}
```

运行以上代码，输出将会是`hello world`。首先，我们引入了`stdio.h`头文件，其中包含了`putchar()`函数用于打印字符。同时，也引入了`ctype.h`头文件，其中包含了`tolower()`函数用于将字符转换为小写。在`main()`函数中，我们声明了一个字符串`str`并赋值为`HELLO WORLD`。然后，通过一个循环遍历字符串中的每个字符，并使用`tolower()`函数将其转换为小写后打印出来。最后，我们使用`printf()`函数打印出换行符来让输出更加清晰。


## 深入了解

在C语言中，所有的字符都是按照ASCII码来表示的。其中，大写字母的ASCII码是65-90，小写字母的ASCII码则是97-122。因此，通过减去32即可将大写字母转换为小写字母。

另外，为了避免因为输入的字符串中含有特殊字符，导致转换后出现错误，我们可以使用`isalpha()`函数来判断字符是否为字母。如果是字母，则使用`tolower()`将其转换为小写，否则保持原样。

## 参考链接

- [C语言转换字符串为小写字母的方法](https://blog.csdn.net/horizonhao/article/details/8534452)
- [C语言字符函数详解：tolower()](https://www.runoob.com/cprogramming/c-function-tolower.html)
- [C标准函数库参考 - tolower()](http://www.cplusplus.com/reference/cctype/tolower/)

## 参考链接

- [C语言转换字符串为小写字母的方法](https://blog.csdn.net/horizonhao/article/details/8534452)
- [C语言字符函数详解：tolower()](https://www.runoob.com/cprogramming/c-function-tolower.html)
- [C标准函数库参考 - tolower()](http://www.cplusplus.com/reference/cctype/tolower/)