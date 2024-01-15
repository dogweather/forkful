---
title:                "字串大写化"
html_title:           "Arduino: 字串大写化"
simple_title:         "字串大写化"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why（为什么）

为什么要对字符串进行大写字母的转换呢？这在某些情况下是很有用的，比如在需要对用户输入的信息进行格式化时。

## How To（如何操作）

操作很简单，我们只需要用一个循环遍历字符串中的每个字符，然后通过内置函数 `toupper()` 将其转换为大写字母即可。下面是一个简单的示例代码：

```Arduino
String str = "hello world";
for (int i = 0; i < str.length(); i++) {
    str[i] = toupper(str[i]);
}
Serial.println(str);
```
输出结果为："HELLO WORLD"

## Deep Dive（深入了解）

这里需要注意的是，`toupper()` 函数只能操作 ASCII 字符，如果字符串中包含了其他编码的字符，那么转换后可能会出现乱码。另外，我们也可以用 `tolower()` 函数将字符串中的字符转换为小写。

## See Also（参考链接）

- [ASCII字符编码表](https://ascii.cl/)
- [Arduino官方文档](https://www.arduino.cc/reference/en/language/functions/communication/toupper/)
- [C++参考教程](https://www.cplusplus.com/reference/cctype/toupper/)