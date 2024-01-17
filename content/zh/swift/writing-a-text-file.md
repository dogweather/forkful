---
title:                "编写文本文件"
html_title:           "Swift: 编写文本文件"
simple_title:         "编写文本文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件？为什么要写？

文本文件是一种存储文本内容的文件类型，它通常包含字符和行来表示文本的结构。程序员通常会写文本文件来存储和读取数据，例如配置文件、日志文件和数据集。

## 如何操作：

下面是一个示例，展示如何在Swift中写入文本文件，并将数据存储在一个名为`example.txt`的文件中：

```Swift 
let text = "这是一个文本文件的例子。"
let filename = "example.txt"

if let docDir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = docDir.appendingPathComponent(filename)
    
    do {
        try text.write(to: fileURL, atomically: true, encoding: .utf8)
    } catch {
        print("写入文件失败：\(error)")
    }
}
```

运行以上代码后，您将在文档目录中找到名为`example.txt`的文件，其中包含文本“这是一个文本文件的例子。”

## 深度挖掘：

文本文件是计算机系统中最基本的文件类型之一，常用于存储人类可读的文本信息。它可以追溯到计算机的早期发展阶段，现在已经被广泛应用于各种领域，例如网站开发、数据处理和游戏存档。

除了使用文本文件，程序员还可以使用其他类型的文件格式来存储数据，例如二进制文件和数据库。然而，文本文件具有易读易编辑的特点，适用于一些简单的文本数据存储需求。

如果您想深入了解如何使用Swift写入文本文件，建议查看苹果官方文档：https://developer.apple.com/documentation/foundation/filemanager/1412649-contents

## 参考链接：

- 苹果官方文档：https://developer.apple.com/documentation/foundation/filemanager
- 文本文件相关知识：https://zh.wikipedia.org/wiki/文本文件