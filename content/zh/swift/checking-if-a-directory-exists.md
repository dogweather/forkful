---
title:                "Swift: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要检查某个目录是否存在。这可以帮助我们避免出现错误和意外情况，使代码更加健壮和可靠。

## 如何实现

下面是一个示例代码，展示如何在Swift中检查目录是否存在，并输出结果。

```Swift
let fileManager = FileManager.default
let directory = "/Users/Desktop/TestDirectory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: directory, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("目录存在")
    } else {
        print("这不是一个目录")
    }
} else {
    print("目录不存在")
}
```

输出结果可能是：

```
目录存在
```

## 深入了解

在Swift中，我们可以使用FileManager类的fileExists方法来检查目录是否存在。这个方法接受两个参数，第一个是文件路径，第二个是一个布尔值的指针，用于指示目录是否存在。

值得注意的是，该方法只适用于检查本地设备上的目录，无法检查远程服务器上的目录。

## 参考链接

- [Swift文档中的FileManager类说明](https://developer.apple.com/documentation/foundation/filemanager)
- [如何在Swift中检查文件是否存在](https://dev.to/alejandromp/swift-how-to-check-if-a-file-exists-53d5)
- [如何在Swift中创建和检查目录](https://www.hackingwithswift.com/example-code/system/how-to-create-a-directory-if-needed-using-filemanager)

## 参考链接

- [Swift文档中的FileManager类说明](https://developer.apple.com/documentation/foundation/filemanager)
- [如何在Swift中检查文件是否存在](https://dev.to/alejandromp/swift-how-to-check-if-a-file-exists-53d5)
- [如何在Swift中创建和检查目录](https://www.hackingwithswift.com/example-code/system/how-to-create-a-directory-if-needed-using-filemanager)