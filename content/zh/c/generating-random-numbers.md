---
title:                "C: 生成随机数"
programming_language: "C"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要生成随机数？

随机数在编程中是非常有用的。它们可以用来测试程序、生成随机密码、随机选择列表中的项等等。在本文中，我们将向您展示如何在C语言中生成随机数，以及一些关于随机数生成的深入信息。

## 如何生成随机数

要在C语言中生成随机数，我们需要使用一个叫做`rand()`的函数。这个函数会生成一个0到`RAND_MAX`之间的随机整数。`RAND_MAX`是C语言中预定义的一个宏，代表了随机数的最大取值。为了使用该函数，我们需要在程序的开头加上`#include <stdlib.h>`来引入标准库中的随机数函数。

我们来看一个简单的例子，代码如下所示：

```C
#include <stdlib.h>
#include <stdio.h>

int main() {
	int randomNumber = rand() % 10; //生成一个0到9之间的随机数
	printf("%d", randomNumber); //打印随机数
	return 0;
}
```

运行以上代码，我们会得到一个随机数作为输出。每次运行程序，输出的随机数都会不同。

## 深入了解随机数生成

随机数的生成本质上是一个伪随机数生成的过程。这意味着它不是真正意义上的随机数，而是通过一定的算法来生成看起来随机的数。在C语言中，这个算法就是`rand()`函数内部的算法。

另一个让人感兴趣的问题是如何让随机数返回不同的值。为了实现这样的功能，我们需要使用`srand()`函数来设置随机数种子。只要我们给`rand()`函数提供不同的种子，它就会返回不同的随机数序列。

下面我们来看一个可以实现这一点的例子：

```C
#include <stdlib.h>
#include <stdio.h>

int main() {
	for(int i = 0; i < 10; i++) {
		srand(i); //设置不同的随机数种子
		int randomNumber = rand() % 10; //生成一个0到9之间的随机数
		printf("%d", randomNumber); //打印随机数
	}
	return 0;
}
```

运行以上代码，我们会得到10个不同的随机数序列输出，每次运行程序随机数序列都会不同。

# 参考链接

- [C语言中的随机数生成](https://www.programiz.com/c-programming/examples/generate-random)
- [随机数生成器原理](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)
- [如何防止随机数重复](https://www.programming9.com/programs/c-programs/316-how-to-avoid-repeating-the-same-random-number)