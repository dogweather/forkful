---
title:                "生成随机数"
aliases:
- zh/java/generating-random-numbers.md
date:                  2024-01-27T20:34:29.437819-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

生成随机数是指在定义的范围内产生不可预测的序列或单个值。程序员使用这种技术出于多种原因，包括模拟、游戏、安全应用以及采样方法，以在不同条件下测试算法。

## 如何实现：

在Java中，可以通过`java.util`包中的`Random`类，或针对特定用例的`ThreadLocalRandom`和`SecureRandom`类来生成随机数。以下示例展示了如何使用这些类。

### 使用 `Random` 类
`Random`类提供了一种生成简单伪随机数的方法。

```Java
import java.util.Random;

public class RandomExample {
    public static void main(String[] args) {
        Random rand = new Random(); // 创建一个Random对象

        int randInt = rand.nextInt(50); // 生成一个0到49的随机整数
        double randDouble = rand.nextDouble(); // 生成一个0.0到1.0之间的随机双精度浮点数
        boolean randBoolean = rand.nextBoolean(); // 生成一个随机布尔值
        
        System.out.println("随机整数: " + randInt);
        System.out.println("随机双精度浮点数: " + randDouble);
        System.out.println("随机布尔值: " + randBoolean);
    }
}
```

### 使用 `ThreadLocalRandom` 类
对于并发应用，`ThreadLocalRandom` 比 `Random` 更高效。

```Java
import java.util.concurrent.ThreadLocalRandom;

public class ThreadLocalRandomExample {
    public static void main(String[] args) {
        int randInt = ThreadLocalRandom.current().nextInt(1, 101); // 从1到100
        double randDouble = ThreadLocalRandom.current().nextDouble(1.0, 10.0); // 从1.0到10.0
        
        System.out.println("随机整数: " + randInt);
        System.out.println("随机双精度浮点数: " + randDouble);
    }
}
```

### 使用 `SecureRandom` 类
对于加密操作，`SecureRandom` 提供了更高级别的安全性。

```Java
import java.security.SecureRandom;

public class SecureRandomExample {
    public static void main(String[] args) {
        SecureRandom secRand = new SecureRandom();
        
        byte[] bytes = new byte[20];
        secRand.nextBytes(bytes); // 使用安全随机数填充字节数组
        
        System.out.println("安全随机字节:");
        for (byte b : bytes) {
            System.out.printf("%02x ", b);
        }
    }
}
```

## 深入探讨

自计算机早期以来，随机数生成已经大大进步。Java的`Random`类使用线性同余公式来生成伪随机数，这些数是确定性的，不适用于高安全应用。这促成了`SecureRandom`的引入，它使用更复杂的算法（例如, SHA1PRNG）来产生密码学上强健的随机数。

然而，`Random`和`SecureRandom`在多线程环境中存在性能下降等缺陷。`ThreadLocalRandom`类在Java 7中被引入，以解决这个问题，它通过提供线程本地随机数生成器，显著提高并发应用中的性能。

虽然这些类满足了大多数需求，但对于极高规模或特殊需求，开发者可能会探索其他库或开发自定义解决方案。基于用例的安全需求和性能要求，选择合适的方法至关重要。
