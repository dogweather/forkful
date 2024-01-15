---
title:                "生成随机数"
html_title:           "Arduino: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么

在编程中，随机数的生成非常有用。随机数可以帮助我们在游戏中创建不同的关卡，或者在实验中模拟不同的情况。使用Arduino编程，我们可以轻松地生成随机数，使我们的项目变得更加有趣和多样化。

# 如何

使用Arduino生成随机数非常简单。只需在代码中使用 "random(min, max)" 函数，其中min是最小值，max是最大值。让我们来创建一个简单的示例，生成一个1-10之间的随机数。

```Arduino
int randomNumber = random(1, 10);
Serial.println(randomNumber);
```

这将在串行监视器中打印出一个随机的整数，例如：5，8，2等等。每次运行代码，都会生成一个新的随机数。

# 深入了解

在编程中，我们常常需要生成真正随机的数字，这样我们的程序才能更加真实和多样化。在Arduino中，我们可以使用 "randomSeed()" 函数来设置随机数种子，从而生成更真实的随机数。

```Arduino
randomSeed(analogRead(A0));  // 使用A0引脚上的模拟值作为种子
int randomNum = random(1, 10);  // 生成一个1-10之间的随机数
Serial.println(randomNum);
```

此外，我们还可以使用 "random(0, n)" 函数来生成一个从0到n-1的随机整数。这可以用于循环中生成随机的索引值，从而创建随机的事件。

# 参考资料

如果想要了解更多关于Arduino中随机数的信息，可以参考以下资源：

- [Arduino随机数函数文档](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [随机数生成器的原理](http://www.instructables.com/id/Introduction-on-How-to-Use-Arduino-Random-Number-/)
- [使用Arduino创建随机游戏](http://www.buildcircuit.com/how-to-create-a-random-number-generator/)

# 查看更多

- [Markdown语法指南](https://www.markdownguide.org/basic-syntax/)
- [Arduino中文官方网站](https://www.arduino.cn/)
- [GitHub上的Arduino代码示例库](https://github.com/arduino/Arduino/blob/master/build/shared/examples/02.Digital/random/Random.ino)