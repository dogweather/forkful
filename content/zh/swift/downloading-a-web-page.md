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

## 为什么

网页是我们日常生活中不可或缺的一部分，有时我们可能想要下载一个网页，无论是为了保存信息还是离线浏览。Swift语言提供了一种简单的方法来实现这一目的，让我们一起来看看如何做到吧！

## 如何做

首先，我们需要引入Foundation框架，它包含了我们需要的NSURLSession类。

```
import Foundation
```

然后，我们需要初始化一个NSURLSession对象，并为其设置一个代理。代理是一个实现NSURLSessionDelegate协议的类，它可以监听下载过程中的不同阶段并传递相关的信息。在这个例子中，我们将简单地打印出下载进度。

```
let session = URLSession(configuration: .default, delegate: self, delegateQueue: .main)
```

接下来，我们需要创建一个URL对象来指定我们要下载的网页。在这里，我们以Apple官网为例。

```
if let url = URL(string: "https://www.apple.com/") {
    let task = session.downloadTask(with: url)
    task.resume()
}
```

注意，我们使用downloadTask方法来创建一个下载任务，并在完成之后调用resume方法来开始下载。现在，让我们来实现代理方法来输出下载进度。

```
extension ViewController: URLSessionDelegate {

    func urlSession(_ session: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo location: URL) {
        print("下载完成")
    }

    func urlSession(_ session: URLSession, task: URLSessionTask, didCompleteWithError error: Error?) {
        if let error = error {
            print("下载出错：\(error.localizedDescription)")
        }
    }
}
```

运行我们的代码，你将会看到在控制台输出下载完成的信息。当然，在实际的应用中，我们还可以利用下载任务返回的URL对象来获取和保存下载下来的数据，这超出了本文的范围。

## 深入了解

这里还有一些有用的知识点，可以帮助你更深入理解下载网页的过程。

首先，我们使用的是NSURLSession类来进行下载，它提供了三种不同的下载任务：dataTask、downloadTask和uploadTask。文中我们使用的是downloadTask，它专门用于下载大型文件。如果我们需要下载普通的数据，比如JSON，那么可以使用dataTask任务。

其次，下载任务会在后台线程中执行，因此需要我们根据需要在代理方法中切换到主线程来更新UI。

最后，我们可以设置NSURLSessionConfiguration对象来配置我们的下载任务，例如设置缓存策略、超时时间、最大并发数等等。详情请参考苹果官方文档。

## 参考链接

- [Apple官方文档（英文）](https://developer.apple.com/documentation/foundation/urlsession)
- [廖师兄的Swift教程（中文）](https://www.liaoxuefeng.com/wiki/1252599548343744/1305201183390114)