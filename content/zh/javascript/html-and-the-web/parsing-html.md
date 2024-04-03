---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 03:01:15.913826-07:00
description: "\u5982\u4F55\u8FDB\u884C\uFF1A \u8BA9\u6211\u4EEC\u4F7F\u7528 JavaScript\
  \ \u4E2D\u7684 `DOMParser` API \u6765\u89E3\u6790 HTML\u3002"
lastmod: '2024-03-13T22:44:48.203143-06:00'
model: gpt-4-0125-preview
summary: "\u8BA9\u6211\u4EEC\u4F7F\u7528 JavaScript \u4E2D\u7684 `DOMParser` API \u6765\
  \u89E3\u6790 HTML."
title: "\u89E3\u6790HTML"
weight: 43
---

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
