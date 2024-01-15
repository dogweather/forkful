---
title:                "读取命令行参数"
html_title:           "Arduino: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

为什么要读取命令行参数：

读取命令行参数是一种常用的编程技巧，它能让我们的程序更加灵活和易于控制。通过读取用户输入的命令行参数，我们可以根据不同的需求执行不同的功能，从而提升程序的效率和可用性。

如何读取命令行参数：

在Arduino中，读取命令行参数是通过利用Serial对象的serialEvent()函数实现的。我们首先需要定义一个字符数组来存储命令行参数，然后在serialEvent()函数中使用if语句来判断是否有新的命令行参数输入，并将其赋值给字符数组。下面是一个简单的示例代码：

```Arduino
char command[10]; 
void serialEvent(){
  if (Serial.available()){
    Serial.readBytesUntil('\n', command, 10);
    // do something with command
  }
}
```

当用户在串口监视器中输入命令行参数并发送后，serialEvent()函数会自动被调用，从而读取并处理用户输入的命令行参数。此外，我们还可以使用Serial.parseInt()函数来读取数字类型的命令行参数，例如：

```Arduino
int num;
void serialEvent(){
  if (Serial.available()){
    num = Serial.parseInt();
    // do something with num
  }
}
```

深入了解命令行参数：

在Arduino中，命令行参数通常以空格为分隔符，且需要以换行符\n结尾才能被正确读取。此外，如果想要读取多个命令行参数，可以使用Serial.readStringUntil()函数来读取指定的分隔符之间的内容。例如，如果想要读取三个参数，可以使用如下代码：

```Arduino
String param1, param2, param3;
void serialEvent(){
  if (Serial.available()){
    param1 = Serial.readStringUntil(' ');
    param2 = Serial.readStringUntil(' ');
    param3 = Serial.readStringUntil('\n');
    // do something with param1, param2, param3
  }
}
```

另外，我们还可以使用Serial.find()函数来判断用户输入的命令行参数中是否包含某个特定的字符，从而增加程序的灵活性。更多关于读取命令行参数的详细信息，请参考官方文档。

参考链接：

- [Arduino官方文档](https://www.arduino.cc/reference/en/language/functions/communication/serial/serial/)
- [阮一峰的博客-Serial.parseInt()函数](http://www.ruanyifeng.com/blog/2017/08/serial-interface.html)
- [Arduino语言参考手册-Serial.readStringUntil()函数](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)

其他相关：

请参考下列链接了解更多关于Arduino编程的知识：

- [为什么要学习Arduino？](https://www.electronicshub.org/why-should-you-learn-arduino/)
- [Arduino编程入门指南](https://www.arduino.cc/en/Tutorial/HomePage)
- [使用Arduino进行物联网开发](https://www.makeuseof.com/tag/getting-started-with-arduino-a-beginners-guide/)