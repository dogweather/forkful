---
title:                "打印调试输出"
html_title:           "Arduino: 打印调试输出"
simple_title:         "打印调试输出"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## 为什么

如果你正在使用Arduino（最新版本）编程，你可能会遇到一些错误和问题。打印调试输出是一种有效的方法，可以帮助你识别问题所在并进行调试，从而更快地解决问题。

## 如何做

首先，在你的代码中插入下面的代码行，以启用调试模式：

```Arduino
#define DEBUG true
```

然后，在需要检查变量值的地方插入下面的代码行：

```Arduino
Serial.println("名称： " + String(变量名));
```

**例如：**

```Arduino
#define DEBUG true

int age = 25;

Serial.println("年龄： " + String(age));
```

运行代码后，你将在串行监控器中看到以下输出：

```
年龄： 25
```

这将告诉你目前的年龄是25岁，方便你进行调试。

## 深入探讨

打印调试输出的一个重要作用是帮助你了解程序中正在发生的事情。它可以帮助你理解变量的值如何在不同的代码行之间变化，以及每个代码行的执行顺序。这可以帮助你更好地理解程序的运行逻辑，并更轻松地调试错误。

另外，调试输出也可以帮助你识别代码中的潜在问题。通过检查输出，你可以看到程序是否按照预期执行，如果有任何不符合预期的情况，就可以迅速发现并解决问题。

## 参考资料

- [Arduino官方网站](https://www.arduino.cc/)
- [Arduino文档](https://www.arduino.cc/reference/en/)
- [为Arduino添加调试功能的方法](https://www.arduino.cc/en/Tutorial/Debugging)
- [如何使用调试输出进行调试](https://learn.sparkfun.com/tutorials/debugging-with-call-outs/all)
- [调试技巧和建议](https://www.arduinoprog.com/Troubleshooting/Arduino_Debugging.html)

## 参考链接

- https://www.arduino.cc/
- https://www.arduino.cc/reference/en/
- https://www.arduino.cc/en/Tutorial/Debugging
- https://learn.sparkfun.com/tutorials/debugging-with-call-outs/all
- https://www.arduinoprog.com/Troubleshooting/Arduino_Debugging.html