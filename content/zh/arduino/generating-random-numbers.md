---
title:    "Arduino: 生成随机数"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# 为什么使用随机数

随机数在编程中扮演着重要的角色，它们可以帮助我们解决许多问题，例如生成密码、模拟随机事件等。通过使用 Arduino，我们可以轻松地生成随机数来解决这些问题。

# 如何生成随机数

要在 Arduino 中生成随机数，我们需要使用 `random(min, max)` 函数。这个函数可以帮助我们指定随机数的范围，如果只想要一个随机数，可以将 `min` 和 `max` 设置为相同的值。下面是一个简单的例子：

```Arduino
int randomNumber = random(1, 10);
```

这将在 1-10 的范围内生成一个随机数，并将其存储在 `randomNumber` 变量中。我们可以使用 `Serial.println(randomNumber)` 将随机数打印出来，如下所示：

```Arduino
int randomNumber = random(1, 10);
Serial.println(randomNumber);
```

这将输出类似于 `6` 的随机数。

我们还可以使用 `randomSeed(seed)` 函数来设置随机数的种子，这可以帮助我们在每次运行代码时生成不同的随机数。例如，我们可以使用 `analogRead(A0)` 来采集模拟引脚 A0 的值，并将其作为种子，如下所示：

```Arduino
randomSeed(analogRead(A0));
int randomNumber = random(1, 10);
Serial.println(randomNumber);
```

这将在每次运行代码时，都使用不同的种子来生成随机数。

# 深入探讨

Arduino 中生成随机数的方法是通过使用伪随机数生成器（PRNG）来实现的。PRNG 是一种算法，它可以根据一个称作“种子”的初始值生成一个看似随机的序列。因此，当我们使用 `randomSeed()` 函数来设置种子时，就会影响到 `random()` 函数生成的随机数。

但是，有时候我们并不需要真正的随机数，而只需要一些“看起来像”随机数的序列。这种情况下，我们可以使用 `random()` 函数来生成伪随机数。

另外，如果需要更高质量的随机数，我们也可以使用外部的随机数源，例如熵源芯片或随机数生成器模块。

# 参考资料

- [Arduino Docs - Random](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Sparkfun - Random Numbers](https://learn.sparkfun.com/tutorials/random-numbers/all)
- [Adafruit - Generating Random Numbers with Arduino](https://learn.adafruit.com/make-it-snow-machine/random-numbers)