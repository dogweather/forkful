---
title:                "“写入标准错误”"
html_title:           "Arduino: “写入标准错误”"
simple_title:         "“写入标准错误”"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 什么是标准错误？为什么程序员需要它？

标准错误是程序员用来调试程序并显示错误消息的一种方法。它允许开发者在程序运行时向终端输出错误消息，而不是程序崩溃或输出无用信息。这样可以帮助程序员快速定位和解决问题，提高程序稳定性和可靠性。

# 如何使用标准错误：

在 Arduino 中，我们可以使用 `Serial` 对象来输出错误消息到终端。下面是一个示例代码：

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int num = 10 / 0; // 这里故意写了一个错误的表达式，会导致程序崩溃
  String errorMessage = "Error: Cannot divide by zero."; // 错误消息
  Serial.println(errorMessage); // 输出错误消息到终端
}
```

运行上面的代码，你会在终端看到错误消息 `Error: Cannot divide by zero.`。

# 深入了解：

在编程领域，标准错误是一种常见的调试技术。它可以追溯到 1960 年代的 UNIX 系统，当时它被用来处理程序崩溃时的错误消息。现在，许多编程语言都支持标准错误，例如 C、C++、Java等。

作为标准错误的替代品，程序员也可以使用 `Serial.println()` 函数来输出消息。不过，这种方法只能用于调试，而不能输出系统错误消息。因此，使用标准错误可以提供更强大的调试功能。

关于标准错误的实现细节，它实际上是一个单独的输出流，通常是 stderr。程序通过向这个流写入数据来输出错误消息，使用标准错误可以引入一些性能开销，但是这是值得的，因为它可以帮助程序员更好地调试程序。

# 相关阅读：

了解更多关于标准错误的背景和用法，可以参考下面的链接：

- [What is Standard Error? - GeeksforGeeks](https://www.geeksforgeeks.org/what-is-standard-error/)
- [Standard error (computing) - Wikipedia](https://en.wikipedia.org/wiki/Standard_error_(computing))
- [Serial.println() - Arduino Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)