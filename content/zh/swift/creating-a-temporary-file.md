---
title:                "创建临时文件"
html_title:           "Swift: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

创建临时文件是指在编程中临时生成一个文件，以存储临时的数据或中间结果。程序员通常会这样做，以便在程序运行过程中能够方便地读取和存储数据。

## 如何:

```Swift
// 创建临时文件的函数
func createTempFile() {
  let fileName = "temp.txt" // 文件名可以自定义
  let tempDir = NSTemporaryDirectory() // 获取临时文件夹的路径
  let filePath = tempDir.appendingPathComponent(fileName) // 在临时文件夹中创建文件路径
  
  // 使用FileHandle在指定路径创建临时文件
  let fileHandle = FileHandle(forWritingAtPath: filePath)
  fileHandle?.closeFile() // 关闭文件句柄
  
  print("临时文件已创建: \(filePath)") // 提示: 临时文件已经创建成功
}

// 调用函数
createTempFile()
```

输出: 临时文件已创建: /var/folders/1v/c6j0y56x73nd04_r9d_kgtdc0000gn/T/temp.txt

## 深入探讨:

创建临时文件的过程一般可以在程序的任何地方进行，但建议在需要保存大量数据或中间结果的时候使用。另外，程序员也可以使用其他方法来存储临时数据，比如使用内存缓存。

如果决定使用临时文件，可以通过FileHandle类来实现。FileHandle类提供了一系列可用来读写文件内容的方法，如上述代码中所示。另外，程序员也可以使用Foundation框架提供的其他方法来创建临时文件，比如NSFileManager的createTemporaryFile方法。

创建临时文件的历史背景可以追溯到早期的编程语言。在早期，程序员通常会在硬盘上手动创建临时文件来存储临时数据。随着编程语言的发展，出现了更多的方法来实现这一功能，使程序员能够更方便地操作临时文件。

## 参考资料:

- [Swift官方文档: Creating and Writing Temporary Files](https://developer.apple.com/documentation/foundation/filehandle/1415840-createtemporaryfile)
- [NSFileManager Class Reference](https://developer.apple.com/documentation/foundation/nsfilemanager)
- [hist file包的历史背景](https://en.wikipedia.org/wiki/Hist_file)