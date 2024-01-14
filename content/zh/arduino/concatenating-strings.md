---
title:    "Arduino: 连接字符串"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么

Arduino编程中，我们经常需要在程序中连接（concatenate）字符串。这可以帮助我们更方便地处理文本信息，比如显示数据或者提示信息。

# 如何

连接字符串需要用到`+`运算符，就像我们在数学中使用的那样。下面是一个简单的例子，我们将两个字符串连接起来，然后将结果输出到串口监视器中：

```Arduino
// 定义两个字符串变量
String str1 = "Hello";
String str2 = "world";

// 将它们连接起来并输出
Serial.println(str1 + str2);
```

运行这段代码，你将看到串口监视器中显示出`Hello world`这个字符串。

除了直接连接两个字符串，我们还可以将数字和字符串一起连接。例如，下面的代码将会将一个整数和一个字符串连接起来：

```Arduino
// 定义一个整数变量和一个字符串变量
int num = 42;
String str = "The answer is ";

// 将它们连接起来并输出
Serial.println(str + num);
```

运行后，你将会看到串口监视器中显示出`The answer is 42`这个字符串。

# 深入探讨

Arduino编程中的字符串连接有一个特殊的地方，那就是字符串变量的声明和赋值必须使用`String`类型，而不是`char`类型。这是因为Arduino默认的字符串类型是`String`，而不是C语言中常用的`char`类型。

另外，如果你希望在连接字符串时不产生空格，可以使用`+=`运算符，它可以在不断构建字符串的过程中继续添加内容，而不是像`+`运算符那样每次都新建一个字符串。

# 参考资料

- [Arduino Reference - String concatenation](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [Arduino常用代码示例大全](https://shumeipai.nxez.com/2014/11/27/arduino-programs.html)
- [C语言中连接字符串的方法](https://www.codingame.com/playgrounds/14213/how-to-play-with-strings-in-c/string-concatenation)

# 参见

[字符串相关的操作指南](https://www.arduino.cn/thread-35635-1-1.html)
[C语言中的字符串操作](https://www.runoob.com/cprogramming/c-standard-library-string-h.html)