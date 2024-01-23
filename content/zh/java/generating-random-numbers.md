---
title:                "生成随机数"
date:                  2024-01-20T17:49:37.935928-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Java"
category:             "Java"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)

生成随机数意味着让计算机产生一个预测不到的数值。程序员会用它来实现加密、模拟、游戏设计，或者在程序中引入随机性，以测试或提高用户体验。

## How to: (怎么做：)

```java
import java.util.Random;

public class RandomNumberExample {
    public static void main(String[] args) {
        Random random = new Random();                   // 创建Random对象实例
        
        int randomInt = random.nextInt(100);            // 随机产生一个0到99的整数
        System.out.println("随机整数: " + randomInt);   // 输出随机整数

        double randomDouble = random.nextDouble();      // 随机产生一个0.0到1.0之间的双精度数
        System.out.println("随机双精度数: " + randomDouble); // 输出随机双精度数
    }
}
```

输出样例：

```
随机整数: 42
随机双精度数: 0.730967787376657
```

## Deep Dive (深入剖析)

随机数生成功能由早期编程语言引入。在Java中，`java.util.Random`类自1.0版本就存在，提供了基本的随机数生成。现在，我们更常用`ThreadLocalRandom`，用于并发场景；和`SecureRandom`，用于安全敏感的应用。

随机数可能从“伪随机”（由算法决定的、有一定预测性）到“真随机”（基于物理过程，无预测性）。`Random`类产生的是伪随机数，够用于大多数非安全相关的应用。

Java 7引入了`ThreadLocalRandom`，解决了多线程环境下随机数生成的性能问题。而对于需要更高安全级别的场景，`SecureRandom`则有特别的算法确保随机数的不可预测性。

## See Also (另请参见)：

- [`Random`类官方文档](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Random.html)
- [`SecureRandom`类官方文档](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/security/SecureRandom.html)
- [`ThreadLocalRandom`类官方文档](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/concurrent/ThreadLocalRandom.html)
- [Oracle的Java加密架构](https://docs.oracle.com/javase/8/docs/technotes/guides/security/crypto/CryptoSpec.html)
- [维基百科上的伪随机数生成器](https://zh.wikipedia.org/wiki/伪随机数生成器)
