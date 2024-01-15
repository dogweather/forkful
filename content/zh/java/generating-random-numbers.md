---
title:                "生成随机数"
html_title:           "Java: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

随机数在Java编程中扮演着重要的角色。它们可以被用来模拟真实世界的随机事件，也可以被用来加密敏感信息。

## 怎么做

生成随机数的方法在Java中有多种。其中最简单的方法是使用Java的内置类Random。下面是一个简单的示例代码：

```Java
// 导入Random类
import java.util.Random;

// 创建Random对象
Random random = new Random();

// 生成一个0-10之间的随机整数
int number = random.nextInt(11);

// 输出结果
System.out.println(number); // 可能输出：5
```

可以发现，每次运行代码，输出结果都会不一样，这就是随机数的特点。除了使用nextInt()方法生成随机整数，还可以使用nextDouble()方法生成随机小数。

## 深入了解

Random类是一个伪随机数生成器，它不是真正意义上的随机，而是通过算法来模拟随机性。如果需要更高质量的随机数，可以使用SecureRandom类，在安全性方面会更加可靠。

另外，Java 8中还引入了新的Random类，名为ThreadLocalRandom。它是线程安全的，可以在并发编程中使用。同时，它也提供了一些更方便的方法来生成随机数。

## 参考资料

- [Java Random类文档](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Java ThreadLocalRandom类文档](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)