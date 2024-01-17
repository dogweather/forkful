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

什么是生成随机数字，为什么程序员要这样做？

生成随机数字是指在代码中使用随机算法来产生随机数的过程。程序员经常使用这种技术来模拟现实生活中的随机事件，例如抽奖、游戏等。这样可以使程序更具有趣味性和变化性。

如何生成随机数字：

```Java
// 导入Java自带的Random类
import java.util.Random;

// 创建随机数生成器对象
Random rand = new Random();

// 生成一个0到10的随机整数
int num = rand.nextInt(11);

// 生成一个0到1的随机浮点数
double num2 = rand.nextDouble();

// 输出结果
System.out.println("随机整数：" + num);
System.out.println("随机浮点数：" + num2);

// 示例输出：
// 随机整数：7
// 随机浮点数：0.829

```

深入了解：

生成随机数的历史背景可以追溯到20世纪50年代，在计算机科学发展初期，人们就开始研究如何产生随机数。目前常见的随机数生成方法有伪随机数和真随机数。伪随机数是由一个确定的算法按照一定规律产生的数列，而真随机数则是由物理现象提供的完全随机的数列。

除了使用Java自带的Random类生成随机数，还有一些其他的方法，例如使用随机数生成器类库。此外，一些编程语言也提供了更强大的随机数生成功能，可以生成更复杂的数字模式。

相关资源：

了解Java自带的Random类：https://docs.oracle.com/javase/8/docs/api/java/util/Random.html

查看其他随机数生成方法：https://dzone.com/articles/getting-random-numbers-in-java

学习各种编程语言中的随机数生成：https://www.baeldung.com/java-random-numbers