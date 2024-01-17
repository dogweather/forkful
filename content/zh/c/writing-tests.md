---
title:                "编写测试"
html_title:           "C: 编写测试"
simple_title:         "编写测试"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-tests.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

编写测试是指在编写程序代码时，我们额外编写一些代码来验证我们的程序是否如预期般正常运行。程序员们这样做的原因是为了确保他们编写的代码能够稳定地运行，并且能够尽可能减少程序中的错误。

## 如何：

```C
#include <stdio.h>

int main() {
  int num1 = 5;
  int num2 = 10;
  int result = num1 + num2;
  printf("The sum of %d and %d is %d\n", num1, num2, result);
  return 0;
}
```

输出：

`The sum of 5 and 10 is 15`

## 深入了解：

编写测试在软件开发中扮演着重要的角色。它可以帮助程序员及早发现潜在的错误，并且可以提供一个可靠的方式来验证程序是否按照预期运行。除了编写测试，还有其他一些方法来验证程序的正确性，例如手动测试和静态代码分析。然而，编写测试仍然是一种值得推荐的做法，因为它可以帮助程序员节省时间和精力，并且可以提高程序的质量。

## 参考资料：

- [C test suite - Learn C programming language](https://beginnersbook.com/2014/01/c-test-suite/)
- [Introduction to software testing - Wikipedia](https://en.wikipedia.org/wiki/Software_testing)