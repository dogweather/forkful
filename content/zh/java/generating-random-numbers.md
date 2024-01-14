---
title:                "Java: 生成随机数"
programming_language: "Java"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么：

随机数在编程中扮演着重要的角色。他们能够帮助我们模拟现实世界中的随机事件，使程序更加有趣和真实。同时，随机数也可以用来加密和保护数据，保护我们的隐私。因此，了解如何生成随机数是非常有用的技能。

## 如何：

在Java中，我们可以使用`Math.random()`方法来生成伪随机数。这个方法会返回一个0到1之间的double类型的随机数。下面是一个简单的例子：

```Java
double number = Math.random();
System.out.println(number);
```

运行以上代码，你会得到类似于以下的输出：

```Java
0.721839209
```

如果我们想要生成一个指定范围内的随机数，我们可以结合使用`Math.random()`和数学运算符。例如，如果我们想要生成一个1到100之间的随机整数，我们可以使用以下代码：

```Java
int num = (int) (Math.random() * 100) + 1;
System.out.println(num);
```

运行以上代码，你会得到一个1到100之间的不同的随机整数。

我们还可以使用其他的方法来生成随机数，比如使用`Random`类或者第三方库。但是无论我们使用哪种方法，我们都需要了解随机数的特性，避免出现错误的结果。

## 深入了解：

虽然我们可以通过多种方法来生成随机数，但是实际上，计算机无法真正地生成随机数。它们只能生成伪随机数，也就是似乎随机，但其实是在按照一定的算法生成的数。因此，在编写程序时，我们需要选择合适的方法和算法来生成随机数，以避免出现重复或不符合要求的结果。

此外，随机数也有一些常见的应用场景，比如游戏开发、密码学和模拟实验等。了解这些应用场景可以帮助我们更好地理解随机数的重要性，同时也可以帮助我们在编程中更加灵活地运用随机数。

## 参考资料：

- [Java Math类](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- [Java随机数生成方法](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)
- [随机数的定义和特性](https://en.wikipedia.org/wiki/Random_number_generation)
- [随机数的应用场景](https://www.guru99.com/random-forest-algorithm.html)

## 参见：

[Java中常用的数学方法](https://github.com/javadriven/JavaMath)