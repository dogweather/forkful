---
title:                "C: 将字符串转换为小写"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##为什么
在编程中，有时候我们需要将字符串转换为小写格式。这可能是为了比较字符串是否相同，或者为了统一格式。无论是什么原因，将字符串转换为小写是一个重要的编程技巧。

##如何做
在C语言中，有几种方法可以将字符串转换为小写格式。

###方法1：使用库函数
C语言提供了一个名为`tolower()`的库函数，可以将一个字符转换为小写格式。我们可以使用一个循环，逐个将字符串中的字符传递给`tolower()`函数，从而实现将整个字符串转换为小写格式的功能。下面是代码示例：

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char str[] = "Hello WORLD";
  
  // 使用循环逐个转换字符
  for (int i = 0; str[i] != '\0'; i++) {
    str[i] = tolower(str[i]);
  }
  
  // 输出转换后的字符串
  printf("转换后的字符串: %s", str);
  
  return 0;
}

```

输出:

`转换后的字符串: hello world`

###方法2：使用指针操作字符串
C语言中，我们可以使用指针来操作字符串。通过遍历字符串，我们可以将每个字符转换为小写格式。下面是代码示例：

```C
#include <stdio.h>

int main() {
  char str[] = "Hello WORLD";
  
  // 使用指针遍历字符串
  char *ptr = str;
  while (*ptr != '\0') {
    if (*ptr >= 'A' && *ptr <= 'Z') {
      // 将大写字母转换为小写
      *ptr = *ptr + 32;
    }
    ptr++;
  }
  
  // 输出转换后的字符串
  printf("转换后的字符串: %s", str);
  
  return 0;
}

```

输出:

`转换后的字符串: hello world`

##深入了解
在C语言中，字符串是以`'\0'`字符结尾的字符数组。因此，我们可以通过遍历数组的方式来操作字符串。在上述的两种方法中，我们都是通过遍历字符串，逐个将字符转换为小写形式。值得注意的是，在使用`tolower()`进行转换时，需要将字符进行强制类型转换，否则可能会出现错误。

##另请参阅
- [C语言字符串操作指南](https://www.runoob.com/cprogramming/c-standard-library-template-macros.html)
- [C语言中各种数据类型的强制类型转换](https://www.runoob.com/cprogramming/c-type-casting.html)