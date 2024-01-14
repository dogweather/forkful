---
title:    "Arduino: 读取命令行参数"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## 为什么要阅读命令行参数
在使用Arduino进行编程时，我们可能会经常遇到需要从命令行中读取参数的情况。这些参数可以提供给我们的程序更多的灵活性和可配置性，帮助我们更好地控制和调试我们的代码。因此，阅读命令行参数是一个非常有用的技能，可以帮助我们更好地理解和运用Arduino。

## 如何阅读命令行参数
阅读命令行参数的最基本的方法是使用Arduino的`Serial`库。我们可以将命令行参数作为字符串通过串口传输给Arduino，并在代码中使用`Serial.readString()`来读取这些参数。下面是一个简单的例子：

```Arduino
// 设置串口通信的波特率为9600
Serial.begin(9600); 

// 在代码中使用Serial.readString()读取命令行参数
String parameter = Serial.readString();

// 将参数打印出来
Serial.println(parameter);
```

假设我们通过串口向Arduino传输参数`hello`，那么在串口监视器中我们将会看到输出`hello`。通过这种方法，我们可以很方便地从命令行中读取参数，并在代码中使用它们。

## 深入了解命令行参数
除了基本的方法外，我们还可以使用`argc`和`argv`这两个变量来读取命令行参数。`argc`代表命令行参数的数量，`argv`则是一个字符串数组，包含了所有的参数。我们可以通过遍历`argv`数组来获取每个单独的参数。下面是一个例子：

```Arduino
// 将命令行参数的数量保存到变量argc中
int argc = ParameterCount();

// 使用for循环遍历argv数组并打印每个参数
for (int i = 0; i < argc; i++) {
    Serial.println(argv[i]);
}
```

如果我们通过串口向Arduino传输参数`hello`和`world`，那么在串口监视器中我们将会看到输出`hello`和`world`。通过使用`argc`和`argv`，我们可以更灵活地读取和处理命令行参数。

## 参考资料
- [Arduino官方文档 - Serial.readString()](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstring/)
- [Arduino官方文档 - ParameterCount()](https://www.arduino.cc/reference/en/language/functions/communication/serial/parametercount/)
- [Arduino官方论坛 - How to read command line arguments](https://forum.arduino.cc/index.php?topic=319866.0)

## 参见
- [Arduino官方文档 - Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [CSDN博客 - Arduino命令行参数的使用](https://blog.csdn.net/csdn_aiyang/article/details/90401563)