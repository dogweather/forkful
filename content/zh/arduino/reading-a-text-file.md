---
title:                "Arduino: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

为什么：为什么阅读文本文件是有意义的？

阅读文本文件是一个有用的技能，因为它允许我们从文本文件中获取和处理数据。在Arduino编程中，我们经常需要从外部源获取数据，并对其进行处理，因此学习如何读取文本文件在许多项目中都是非常重要的。

如何：在Arduino中读取文本文件的方法非常简单。首先，我们需要确定文本文件的位置，然后打开文件，并使用`Serial.println()`函数将文本内容打印到串行监视器中。

```Arduino
File file = SD.open("/textfile.txt"); // 打开文本文件
while (file.available()) { // 循环读取文件内容
    Serial.println(file.readStringUntil('\n'));
}
file.close(); // 关闭文件
```

输出结果将会是文本文件中每行的内容，这样我们就可以看到文本信息并对其进行后续处理。

深入探讨：除了简单地读取文本文件中的内容，还有许多其他有用的方法。例如，我们可以使用`file.readString()`函数来读取整个文件的内容，而不仅仅是一行。我们还可以通过修改文本文件中的内容来更新数据，并使用`file.println()`函数将更新后的内容保存到文件中。

另外，通过使用字符串函数，我们可以对文本数据进行分割和处理，以便更有效地提取有用的信息。

总之，读取文本文件是非常重要的，因为它允许我们轻松地处理数据并在Arduino项目中使用它们。

参考链接：
- [Arduino官方文档-读取文本文件](https://www.arduino.cc/en/Tutorial/LibraryExamples/CreateFile/Reader)
- [Arduino中文社区-读取文本文件教程](https://www.arduino.cn/thread-16719-1-1.html)
- [CSDN博客-SD卡操作教程](https://blog.csdn.net/tianZHJSC/article/details/50797012)

另见：
- [Markdown官方文档](https://daringfireball.net/projects/markdown/)
- [VSCode支持Markdown插件](https://marketplace.visualstudio.com/items?itemName=shd101wyy.markdown-preview-enhanced)