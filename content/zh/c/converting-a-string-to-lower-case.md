---
title:                "C: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，有时候我们需要将字符串中的字母全部改为小写。这样我们可以更轻松地比较字符串，或者让字符串更易于阅读。在这篇博客文章中，我们将学习如何将字符串转换为小写字母，以及其中的一些技巧。

## 如何做

为了将字符串转换为小写，我们需要使用一个C语言中的内置函数，它叫做`tolower()`。这个函数接受一个字符作为参数，并返回该字符的小写形式。让我们来看一个例子：

```C
#include <stdio.h>
#include <ctype.h>

int main(){
    char str[] = "Hello World!";
    int i = 0;
    
    while(str[i]){
        str[i] = tolower(str[i]);
        i++;
    }
    
    printf("%s", str);
    return 0;
}
```

在上面的代码中，我们首先包含了`stdio.h`和`ctype.h`头文件，以便我们可以使用`printf()`和`tolower()`函数。接下来，我们创建了一个包含“Hello World!”的字符串`str`，并使用`while`循环和`tolower()`函数将每个字符转换为小写。最后，我们使用`printf()`函数打印出最终结果。

运行上面的代码，我们会得到以下输出：

```C
hello world!
```

总的来说，将字符串转换为小写的过程就是遍历字符串中的每个字符并使用`tolower()`函数将其转换为小写字母后重新覆盖原来的字符。

## 深入了解

除了使用`tolower()`函数，我们还可以使用条件语句和`ascii`码来实现字符串转换。每个字符都有一个对应的`ascii`码，其中包含大小写字母和其他特殊字符。我们可以根据`ascii`码的范围来判断一个字符是否为大写字母，并在需要的情况下将其转换为小写。同时，我们还可以使用循环来遍历字符串中的每个字符，这样就可以实现相同的效果，而不用使用`strcpy()`函数。

除了将字符串转换为小写，我们还可以使用相同的方法将字符串转换为大写。只需要将`tolower()`函数改为`toupper()`即可。

## 参考链接

- [C语言中的字符函数](https://www.runoob.com/cprogramming/c-library-character-functions.html)
- [ascii码表](https://baike.baidu.com/item/ASCII/309296?fr=aladdin)