---
title:                "建立临时文件"
html_title:           "Arduino: 建立临时文件"
simple_title:         "建立临时文件"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# 创建临时文件

## 什么是临时文件：
创建临时文件是指在程序运行期间，临时生成一个文件来存储数据或者执行某项任务。程序员通常会在需要存储临时数据或者执行一些临时操作时使用临时文件。

## 如何创建临时文件：
创建临时文件可以通过Arduino的File库中的“tmpFile”函数来实现。以下是一个例子：

```
Arduino_File tmpFile;
tmpFile = File.createTempFile("example", ".txt");
```

这段代码将会在Arduino控制台上创建一个名为“example.txt”的临时文件。

## 深入了解：
创建临时文件的历史可以追溯到早期的操作系统，当时程序无法在内存中保存大量的临时数据，因此需要将数据存储在临时文件中。如今，创建临时文件可以是为了避免程序中断时临时数据的丢失，或者是为了执行一些需要保存数据的临时任务。

除了使用Arduino的“tmpFile”函数，程序员还可以使用其他编程语言中的临时文件创建方法来实现相同的效果。例如，在Python中可以使用“tempfile”模块来创建临时文件。

## 相关资源：
- [Arduino官方文档](https://www.arduino.cc/reference/en/libraries/file/)
- [Python官方文档](https://docs.python.org/3/library/tempfile.html)