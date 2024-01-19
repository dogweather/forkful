---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

生成随机数是编程中创建一个预测难度高的数的过程。程序员这么做是因为随机数可以在创建游戏、模拟事件、测试程序效率等方面有所帮助。

## 怎么做：

Java中生成随机数的一种常见方式是使用`java.util.Random`类的实例。让我们看看怎么做：

```Java
import java.util.Random;

public class randomNumbers {
    public static void main(String[] args) {
        // 创建一个新的随机数生成器
        Random rand = new Random();

        // 生成一个介于0和100之间的随机整数
        int randomNum = rand.nextInt(100);
        System.out.println("生成的随机数是: " + randomNum);
    }
}
```
在运行上述程序时，输出可能会是这样（因为产生的是随机数，每次的输出可能会不同）：

```
生成的随机数是: 42
```

## 深入了解：

生成随机数的概念在计算机术语中有着悠久的历史，最早可以追溯到1950年代的早期计算机。虽然大多数现代的随机数生成方法相较于早期方法有了巨大的改进，但理论基础基本一致。

有许多方式可以在Java中生成随机数，包括Math.random()函数、java.security.SecureRandom类等等。选择哪种方式取决于具体的需求–例如，SecureRandom类比Random类提供了更强的安全性，但牺牲了一些性能。

Java的随机数生成实际上产生的是伪随机数，因为它们是根据初始种子值(split)通过计算得出的。尽管这些数值对于大多数用户来说都是“随机的”，但理论上，如果知道了初始种子和生成算法，就可以预测出随机数。

## 参阅：

要了解有关Java生成随机数的更多信息，这里有一些相关的链接：
1. Oracle官方文档: [Random 类](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
2. Baeldung教程: [在Java中生成随机数](https://www.baeldung.com/java-generate-random-long-float-integer-double)
3. Stack Overflow讨论：[为什么Math.random()比Random类优秀](https://stackoverflow.com/questions/738629/math-random-versus-random-nextdouble)