---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:01:15.913826-07:00
description: "\u89E3\u6790 HTML \u6307\u7684\u662F\u4ECE HTML \u6587\u6863\u4E2D\u63D0\
  \u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u4E00\u64CD\u4F5C\u662F\
  \u4E3A\u4E86\u4E0E\u7F51\u9875\u5185\u5BB9\u8FDB\u884C\u4EA4\u4E92\u6216\u64CD\u63A7\
  \u3001\u81EA\u52A8\u5316\u6570\u636E\u63D0\u53D6\uFF0C\u6216\u8005\u7528\u4E8E\u7F51\
  \u7EDC\u6293\u53D6\u76EE\u7684\u3002"
lastmod: '2024-02-25T18:49:45.770736-07:00'
model: gpt-4-0125-preview
summary: "\u89E3\u6790 HTML \u6307\u7684\u662F\u4ECE HTML \u6587\u6863\u4E2D\u63D0\
  \u53D6\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FDB\u884C\u8FD9\u4E00\u64CD\u4F5C\u662F\
  \u4E3A\u4E86\u4E0E\u7F51\u9875\u5185\u5BB9\u8FDB\u884C\u4EA4\u4E92\u6216\u64CD\u63A7\
  \u3001\u81EA\u52A8\u5316\u6570\u636E\u63D0\u53D6\uFF0C\u6216\u8005\u7528\u4E8E\u7F51\
  \u7EDC\u6293\u53D6\u76EE\u7684\u3002"
title: "\u89E3\u6790HTML"
---

{{< edit_this_page >}}

## 什么 & 为什么？
解析 HTML 指的是从 HTML 文档中提取数据。程序员进行这一操作是为了与网页内容进行交互或操控、自动化数据提取，或者用于网络抓取目的。

## 如何进行：
让我们使用 JavaScript 中的 `DOMParser` API 来解析 HTML。

```Javascript
const parser = new DOMParser();
const htmlString = `<p>Hello, world!</p>`;
const doc = parser.parseFromString(htmlString, 'text/html');
console.log(doc.body.textContent); // 输出：Hello, world!
```

现在，让我们来获取一些更具体的内容，比如一个具有类的元素：

```Javascript
const htmlString = `<div><p class="greeting">Hello, again!</p></div>`;
const doc = parser.parseFromString(htmlString, 'text/html');
const greeting = doc.querySelector('.greeting').textContent;
console.log(greeting); // 输出：Hello, again!
```

## 深入了解
解析 HTML 与网络同龄。起初，这是浏览器的事情——浏览器解析 HTML 以显示网页。随着时间的推移，程序员想要介入这一过程，这导致了如 `DOMParser` 这样的 API 的出现。

有替代方案吗？当然。我们有如 `jQuery` 这样的库和Python的 `BeautifulSoup` 工具。但 JavaScript 的原生 `DOMParser` 既快速又内置，不需要额外的库。

在实现方面，当你用 `DOMParser` 解析 HTML 时，它会创建一个 `Document` 对象。可以将其视为你的 HTML 的层次模型。一旦你拥有它，就可以像操作正常网页的 DOM 一样导航和操控它。

但有一点——解析可能会因为格式不良的 HTML 而出错。浏览器可能会包容这些错误，但 `DOMParser` 可能不会。因此，对于复杂的任务或杂乱的 HTML，第三方库可能会做得更好。

## 另请参阅
- MDN Web Docs 关于 `DOMParser` API 的文档：[MDN DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery 的解析功能：[jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Cheerio, 一个快速、灵活且精简的服务端 jQuery 核心实现：[Cheerio.js](https://cheerio.js.org/)
- 对于非 JS 解析：Python 的 BeautifulSoup 库：[Beautiful Soup](https://www.crummy.com/software/BeautifulSoup/)
