---
title:                "检查文件夹是否存在"
html_title:           "Arduino: 检查文件夹是否存在"
simple_title:         "检查文件夹是否存在"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

通常，当我们编写代码时，需要在程序执行期间检查某个目录是否存在。这可以避免程序因为缺少必要的目录而崩溃，从而确保程序的稳定性。

## 如何进行检查

在Arduino中，我们可以使用`SD.exists()`函数来检查SD卡中的目录是否存在。下面是一个示例代码：

```Arduino
if(SD.exists("/photos")){
  Serial.println("The directory 'photos' exists.");
} else {
  Serial.println("The directory 'photos' does not exist.");
}
```

在这个例子中，我们首先使用`exists()`函数来检查名为“photos”的目录是否存在。如果存在，我们会向串口打印一条消息，如果不存在，我们会打印另一个消息。根据程序执行结果，我们可以确定目录的存在状态。

## 深入了解

除了检查目录是否存在，我们还可以通过将目录名作为参数传递给`SD.exists()`函数来检查子目录是否存在。例如，我们可以使用`SD.exists("/photos/travel")`来检查“photos”目录中是否有名为“travel”的子目录。

此外，我们还可以使用`SD.cwd()`函数来获取当前工作目录的路径，并使用`SD.chdir()`函数来更改当前工作目录。这些函数对于在程序中管理目录结构非常有用。

## 参考链接

- [Arduino官方文档：SD.exists()函数](https://www.arduino.cc/en/Reference/SDexists)
- [Arduino官方文档：SD.cwd()函数](https://www.arduino.cc/en/Reference/SDcwd)
- [Arduino官方文档：SD.chdir()函数](https://www.arduino.cc/en/Reference/SDchdir)