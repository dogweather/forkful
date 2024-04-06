---
date: 2024-01-20 17:55:16.730510-07:00
description: "How to: (\u5982\u4F55\u505A\uFF1A) \u5728\u65E9\u671F\u7684\u7F16\u7A0B\
  \u65E5\u5B50\u91CC\uFF0C\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u9700\u8981\u5904\u7406\
  \u5927\u91CF\u7684\u5E95\u5C42\u6587\u4EF6\u64CD\u4F5C\u548C\u9519\u8BEF\u7BA1\u7406\
  \u3002\u968F\u7740\u9AD8\u7EA7\u8BED\u8A00\u5982 Swift \u7684\u51FA\u73B0\uFF0C\u8FD9\
  \u4E00\u8FC7\u7A0B\u88AB\u7B80\u5316\u4E86\u3002\u5728 Swift \u4E2D\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528 `String` \u7C7B\u7684 `contentsOfFile` \u65B9\u6CD5\u6765\u7B80\
  \u6D01\u5730\u8BFB\u53D6\u6587\u672C\u5185\u5BB9\u3002\u5C3D\u7BA1\u8FD9\u4E2A\u65B9\
  \u6CD5\u5F88\u65B9\u4FBF\uFF0C\u4E5F\u6709\u5176\u4ED6\u65B9\u5F0F\u5982\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.383161-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u505A\uFF1A) \u5728\u65E9\u671F\u7684\u7F16\u7A0B\u65E5\u5B50\
  \u91CC\uFF0C\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u9700\u8981\u5904\u7406\u5927\u91CF\
  \u7684\u5E95\u5C42\u6587\u4EF6\u64CD\u4F5C\u548C\u9519\u8BEF\u7BA1\u7406\u3002\u968F\
  \u7740\u9AD8\u7EA7\u8BED\u8A00\u5982 Swift \u7684\u51FA\u73B0\uFF0C\u8FD9\u4E00\u8FC7\
  \u7A0B\u88AB\u7B80\u5316\u4E86\u3002\u5728 Swift \u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\
  \u7528 `String` \u7C7B\u7684 `contentsOfFile` \u65B9\u6CD5\u6765\u7B80\u6D01\u5730\
  \u8BFB\u53D6\u6587\u672C\u5185\u5BB9\u3002\u5C3D\u7BA1\u8FD9\u4E2A\u65B9\u6CD5\u5F88\
  \u65B9\u4FBF\uFF0C\u4E5F\u6709\u5176\u4ED6\u65B9\u5F0F\u5982 `FileHandle` \u6216\
  \u4F7F\u7528 `StreamReader` \u8FDB\u884C\u5927\u6587\u4EF6\u6216\u6D41\u5F0F\u8BFB\
  \u53D6\uFF0C\u8FD9\u6837\u53EF\u4EE5\u66F4\u9AD8\u6548\u5730\u5904\u7406\u5927\u91CF\
  \u6570\u636E\u3002\u5B9E\u73B0\u4E0A\uFF0CSwift \u5185\u90E8\u5C06\u6587\u4EF6\u5185\
  \u5BB9\u8F6C\u5316\u4E3A\u5B57\u7B26\u4E32\uFF0C\u8FD9\u901A\u5E38\u6D89\u53CA\u7F16\
  \u7801\u7684\u8F6C\u6362\uFF08\u5982\u793A\u4F8B\u4E2D\u7684 `.utf8`\uFF09\uFF0C\
  \u8FD9\u662F\u56E0\u4E3A\u6587\u4EF6\u5B58\u50A8\u7684\u662F\u5B57\u8282\u5E8F\u5217\
  \uFF0C\u800C\u6211\u4EEC\u60F3\u8981\u7684\u662F\u8BA9\u4EBA\u53EF\u8BFB\u7684\u6587\
  \u672C\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

## How to: (如何做：)
```Swift
import Foundation

if let path = Bundle.main.path(forResource: "example", ofType: "txt") {
    do {
        let data = try String(contentsOfFile: path, encoding: .utf8)
        print(data)
    } catch {
        print("Oops! 无法读取文件: \(error)")
    }
}
```
输出范例：
```
Hello, this is the content of the example text file.
你好，这是示例文本文件的内容。
```

## Deep Dive (深入探讨)
在早期的编程日子里，读取文本文件需要处理大量的底层文件操作和错误管理。随着高级语言如 Swift 的出现，这一过程被简化了。在 Swift 中，你可以使用 `String` 类的 `contentsOfFile` 方法来简洁地读取文本内容。尽管这个方法很方便，也有其他方式如 `FileHandle` 或使用 `StreamReader` 进行大文件或流式读取，这样可以更高效地处理大量数据。实现上，Swift 内部将文件内容转化为字符串，这通常涉及编码的转换（如示例中的 `.utf8`），这是因为文件存储的是字节序列，而我们想要的是让人可读的文本。

## See Also (另请参阅)
- [Swift Standard Library Documentation](https://developer.apple.com/documentation/swift/swift_standard_library)
- [Apple's FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Book by Apple](https://docs.swift.org/swift-book/)
