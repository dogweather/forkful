---
title:    "Arduino: 读取文本文件"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

#为什么选择读取文本文件
在Arduino编程中，读取文本文件是一个非常有用的功能。它可以帮助你在代码中储存大量的文本信息，从而提高程序的灵活性和可读性。读取文本文件还可以让你轻松地更新和修改代码中的文本内容，而不需要重新编译程序。

#如何实现读取文本文件
首先，我们需要使用Arduino的SD库来读取文本文件。然后，我们需要使用`File`对象来打开和读取文本文件。接下来，我们可以使用`read()`函数来读取文件中的每一个字符，并将其存储在一个数组或字符串中。最后，我们可以使用`while`循环来读取整个文件，直到所有的内容都被读取完毕。

```Arduino
#include <SD.h>
File myFile;

void setup() {
  Serial.begin(9600);
  //初始化SD卡
  if (!SD.begin(4)) {
    Serial.println("SD卡失败！");
  }
  //打开文件，参数为文件名和打开方式（只读）
  myFile = SD.open("data.txt", FILE_READ);
  //如果文件无法打开，则输出错误信息
  if (!myFile) {
    Serial.println("无法打开文件！");
  }
  //读取文件中的字符，并逐个打印
  while (myFile.available()) {
    char c = myFile.read();
    Serial.print(c);
  }
  //关闭文件
  myFile.close();
}

void loop() {
  //你的代码
}
```

输出结果：
```
这是一个测试文件。
```

#深入了解读取文本文件
除了使用`read()`函数，我们还可以使用`readString()`函数来读取整个文件中的一行文本内容。我们可以使用`indexOf()`和`substring()`函数来查找和提取特定内容，从而实现更加复杂的文本处理。

另外，我们也可以在读取文本文件时进行错误检测，例如文件是否打开成功，文件是否存在等。这样可以让我们的程序更加健壮和稳定。

#相关链接
- Arduino SD库文档：https://www.arduino.cc/en/Reference/SD
- `read()`函数文档：https://www.arduino.cc/en/Reference/FileRead
- `readString()`函数文档：https://www.arduino.cc/en/Reference/FileReadString