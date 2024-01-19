---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## 何为解析HTML？为何程序员需要解析？

解析HTML就是利用代码来读取和理解HTML文件的过程，从而使HTML元素和属性变得可操作。程序员需要解析HTML来实现网页数据抓取、内容提取或实现Web自动化等功能。

## 如何实现：

以下是一个用JavaScript解析HTML的基本示例：

```javascript
const { JSDOM } = require("jsdom");

const html = `<p>你好，世界！</p>`;

//利用 JSDOM 解析 HTML
const dom = new JSDOM(html);

console.log(dom.window.document.querySelector("p").textContent); // 输出 "你好，世界！"
```

在上述代码中，我们使用了JSDOM库解析HTML，并提取了其中的文本内容。

## 深入了解

1. 历史背景：HTML解析可以追溯到1990年代World Wide Web的诞生，当时的网页主要是静态HTML，解析HTML可以实现动态网页。

2. 替代方案：除了JSDOM，还有许多 library 如 Beautiful Soup 和 Cheerio 可以实现HTML的解析。选择哪种取决于你的具体需求和使用环境。

3. 实现细节：解析HTML其实就是构建DOM——Document Object Model-一种以对象形式表达HTML和XML文档结构的API。解析器会按照HTML标记（例如`<p>`和`</p>`）生成DOM树。

## 另请参考

- JSDOM官方文档(https://github.com/jsdom/jsdom)
- Mozilla开发者们关于 DOM 的更多信息(https://developer.mozilla.org/zh-CN/docs/Web/API/Document_Object_Model/Introduction)
- HTML、DOM和JavaScript教程(https://www.w3schools.com/js/default.asp)