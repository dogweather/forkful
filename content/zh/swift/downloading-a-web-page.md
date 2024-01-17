---
title:                "下载网页"
html_title:           "Swift: 下载网页"
simple_title:         "下载网页"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
下载网页是将网页从网络上保存到本地设备的过程。程序员通常会这样做是因为他们需要获取网页的内容，分析数据，或者在应用程序中显示网页。

# 如何：
```Swift
let url = URL(string: "https://www.example.com")
if let webpage = try? String(contentsOf: url) {
    print(webpage)
}
```

## 深入探讨：
下载网页最早是通过File Transfer Protocol (FTP)来实现的，但现在大多数网页都使用Hypertext Transfer Protocol (HTTP)。除了使用URL来下载网页，还可以使用更先进的方法，如使用URLSession和DataTask。另外，要注意避免在应用程序中使用大量的网络请求，以避免影响性能。

# 查看更多：
- [URLSession document](https://developer.apple.com/documentation/foundation/urlsession)
- [DataTask document](https://developer.apple.com/documentation/foundation/datatask)