---
title:                "Arduino: 创建临时文件"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

＃＃为什么
为什么会想要创建临时文件？临时文件在Arduino编程中具有很多用处。它们可以用来存储临时数据，保存程序状态，甚至管理连接的设备。它们是实现更复杂功能的基础。 

＃＃如何
在Arduino中创建临时文件非常简单。首先，您需要包含`SD`库。然后，您可以使用`File`对象来创建文件。下面是一个简单的示例代码：

```
Arduino屏幕打印临时文件创建;
文件tempFile=SD打开（“temp.txt”，FILE_WRITE）;
tempFile.println（“这是一个临时文件！”）;
tempFile.close（）;
```

运行此代码，您将在SD卡上创建一个名为“temp.txt”的文件，并将“这是一个临时文件！”写入文件。现在您可以按照自己的需求使用该临时文件了。

＃＃深入了解
默认情况下，Arduino的`File`对象在创建文件时使用`FILE_WRITE`模式，这意味着如果文件已经存在，则会覆盖该文件。您也可以使用`FILE_APPEND`模式来追加数据到现有文件中。此外，您可以在创建文件时指定文件名和路径，以及使用不同的数据类型来写入文件。在尝试创建临时文件时，请确保您的SD卡已经正确连接并初始化，以及您的编程板已经正确地设置了文件系统。

看到
如果您想了解更多关于使用Arduino创建临时文件的信息，您可以参考以下链接：
- [Adafruit的“创建临时文件”教程](https://learn.adafruit.com/adafruit-vsisit-temp-logger-arduino-using-sd-cards/temporary-files) 
- [Arduino文件对象文档](https://www.arduino.cc/en/Reference/SDFileObject) 
- [Sparkfun的“使用SD卡”教程](https://learn.sparkfun.com/tutorials/sd-cards-and-arduino/all)