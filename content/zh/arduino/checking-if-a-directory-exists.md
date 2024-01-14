---
title:                "Arduino: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么
一个常见的问题是，当我们在进行Arduino编程时，我们可能需要检查一个特定的文件夹是否存在。这通常被认为是一种好的做法，因为它可以帮助我们避免因为文件夹不存在而导致的错误。在这篇博客文章中，我们将探讨如何在Arduino中检查文件夹是否存在，以及为什么这样做很重要。

## 如何
要在Arduino中检查文件夹是否存在，我们可以使用`File`库中提供的`exists()`函数。这个函数接受一个字符串参数，即文件夹的路径，并返回一个布尔值，表明文件夹是否存在。让我们来看一个简单的例子：

```Arduino
#include <SD.h>
File myDirectory = SD.open("/myFolder");
if(myDirectory.exists()){
  Serial.println("The directory exists!");
} else {
  Serial.println("The directory does not exist!");
}
```

如果文件夹存在，串行监视器将打印出"The directory exists!"，否则将打印出"The directory does not exist!"。

## 深入探讨
在Arduino中检查文件夹是否存在是一个非常重要的步骤，特别是当我们需要从一个特定的文件夹中读取或写入数据时。通过确保文件夹存在，我们可以避免程序出错，提高代码的可靠性。

`exists()`函数还可以用来检查文件是否存在。如果我们要检查一个特定的文件是否存在，只需将文件的路径传递给`exists()`函数即可。

## 参考链接
- [Arduino File Library Reference](https://www.arduino.cc/en/Reference/SD)
- [How to use exists() function in Arduino](https://maker.pro/arduino/tutorial/arduino-exists-function-usage)
- [Arduino: Checking if a File or Directory Exists](https://techtutorialsx.com/2016/05/01/arduino-how-to-check-if-a-file-or-directory-exists/)