---
aliases:
- /zh/swift/downloading-a-web-page/
date: 2024-01-20 17:44:45.063232-07:00
description: "\u4E0B\u8F7D\u7F51\u9875\u662F\u83B7\u53D6\u7F51\u7AD9\u5185\u5BB9\u7684\
  \u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u8FD9\u6837\u505A\uFF0C\u56E0\u4E3A\
  \u4ED6\u4EEC\u9700\u8981\u8BBF\u95EE\u548C\u4F7F\u7528\u8FD9\u4E9B\u5728\u7EBF\u6570\
  \u636E\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.441139
model: gpt-4-1106-preview
summary: "\u4E0B\u8F7D\u7F51\u9875\u662F\u83B7\u53D6\u7F51\u7AD9\u5185\u5BB9\u7684\
  \u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u8FD9\u6837\u505A\uFF0C\u56E0\u4E3A\
  \u4ED6\u4EEC\u9700\u8981\u8BBF\u95EE\u548C\u4F7F\u7528\u8FD9\u4E9B\u5728\u7EBF\u6570\
  \u636E\u3002"
title: "\u4E0B\u8F7D\u7F51\u9875"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
下载网页是获取网站内容的过程。程序员通常这样做，因为他们需要访问和使用这些在线数据。

## How to: 怎样做？
### 下载文本内容

```Swift
import Foundation

let url = URL(string: "https://www.example.com")!

let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("错误: \(error)")
    } else if let data = data, let string = String(data: data, encoding: .utf8) {
        print(string)
    }
}

task.resume()
```

### 示例输出

```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive 深入了解
下载网页的历史开始于互联网的早期，那时只需简单的HTTP GET 请求。现在，有多种方法来实现：`URLSession` 是 Swift 中的现代方法，适用于完成这项任务。它提供异步API，允许多线程处理，而且它是可定制的。替代方法包括 `NSURLConnection`（现已弃用）、第三方库（如 Alamofire）。

网络请求会涉及响应处理，这就是为什么正确处理回调和错误至关重要。回调中的 `data` 携带着下载内容，`response` 提供了请求的元数据，`error` 则包含了请求过程中出现的任何错误信息。

## See Also 查看更多
- [Swift Standard Library](https://developer.apple.com/documentation/swift)
- [URLSession Documentation](https://developer.apple.com/documentation/foundation/urlsession)
- [Working with URLSession in Swift](https://www.raywenderlich.com/3244963-urlsession-tutorial-getting-started)
