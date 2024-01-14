---
title:                "TypeScript: 下载网页."
simple_title:         "下载网页."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

当你浏览网页时，想必会遇到需要将某个网页下载到本地存储的情况。这可能是因为你想要离线浏览，或者你需要对网页进行分析。无论出于何种原因，下载网页是一项非常常见的任务，并且在 TypeScript 中也能轻松实现。

## 如何

在 TypeScript 中，我们可以使用 `fetch` 方法来下载网页。首先，我们需要创建一个 async 函数来处理下载的逻辑。然后，使用 `fetch` 方法并传入想要下载的网页的 URL。最后，使用 `text` 方法来读取返回的结果。

```TypeScript
async function downloadPage() {
  const response = await fetch('https://www.example.com');
  const html = await response.text();
  console.log(html); // 打印网页内容
}

downloadPage();
```

运行以上代码后，你会发现网页的 HTML 内容被打印出来了。

## 深入了解

下载网页的过程其实并不复杂，主要是通过 HTTP 请求来获取网页的内容。除了 `fetch` 方法外，我们也可以使用 `XMLHttpRequest` 对象来发送 HTTP 请求。使用 `XMLHttpRequest`，我们可以更灵活地控制请求的细节，例如设置请求头部信息或发送 POST 请求。

另外，为了处理可能出现的错误，我们也可以使用 `try/catch` 语句来捕获异常并进行相应的处理。此外，如果你想要保存下载的网页，也可以使用 Node.js 中的 `fs` 模块来将网页内容写入本地文件。

## 查看更多

- [fetch 文档](https://developer.mozilla.org/zh-CN/docs/Web/API/WindowOrWorkerGlobalScope/fetch)
- [XMLHttpRequest 文档](https://developer.mozilla.org/zh-CN/docs/Web/API/XMLHttpRequest)
- [fs 模块文档](https://nodejs.org/api/fs.html)