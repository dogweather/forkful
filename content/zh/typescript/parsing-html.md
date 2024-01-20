---
title:                "解析HTML"
date:                  2024-01-20T15:34:06.284140-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
解析HTML是指将HTML字符串转换成可以编程操作的对象。程序员这么做是为了从网页中提取数据或者操作DOM。

## How to: 怎么做
首先，安装一个第三方库，如`node-html-parser`。然后使用TypeScript解析HTML：

```typescript
import { parse } from 'node-html-parser';

const html = `
  <div>
    <h1>Title</h1>
    <p>Description here</p>
  </div>
`;

const root = parse(html);
console.log(root.querySelector('h1').textContent); // 输出: Title
```

## Deep Dive 深度探索
在1990年代，HTML主要是静态的，简单的字符串操作足以处理。现代开发中，HTML变得复杂，存在大量动态内容。因此，出现了各种HTML解析库，如`cheerio`, `jsdom`, 和刚才提到的`node-html-parser`。这些库使用不同的技术来解析和操作HTML文档。

比起正则表达式，这些库更准确，它们通过DOM树的结构来分析文档，而不是简单地搜索字符串模式。选择哪个库取决于你的具体需求：性能、易用性、功能的全面性等。

## See Also 相关资源
- [MDN Web Docs on HTML](https://developer.mozilla.org/zh-CN/docs/Web/HTML)
- [node-html-parser on npm](https://www.npmjs.com/package/node-html-parser)
- [cheerio GitHub repository](https://github.com/cheeriojs/cheerio)
- [jsdom GitHub repository](https://github.com/jsdom/jsdom)