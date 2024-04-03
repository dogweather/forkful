---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:17.060490-07:00
description: "\u5982\u4F55\u8FDB\u884C: \u9996\u5148\uFF0C\u5B89\u88C5\u50CF`node-html-parser`\u8FD9\
  \u6837\u7684\u5E93\u3002\u8FD9\u662F\u7EC8\u7AEF\u547D\u4EE4\uFF1A."
lastmod: '2024-03-13T22:44:47.469290-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u5B89\u88C5\u50CF`node-html-parser`\u8FD9\u6837\u7684\
  \u5E93\u3002\u8FD9\u662F\u7EC8\u7AEF\u547D\u4EE4\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何进行:
首先，安装像`node-html-parser`这样的库。这是终端命令：

```bash
npm install node-html-parser
```

现在，让我们用TypeScript来解析一些基本的HTML：

```typescript
import { parse } from 'node-html-parser';

const html = `<ul class="fruits">
                <li>Apple</li>
                <li>Banana</li>
              </ul>`;

const root = parse(html);
console.log(root.querySelector('.fruits').textContent);  // "Apple Banana"
```

如果你只想抓住香蕉：

```typescript
const bananas = root.querySelectorAll('li')[1].textContent;
console.log(bananas);  // "Banana"
```

## 深入了解
解析HTML并不是新事物——自网络早期以来就已存在。起初，开发者可能使用了正则表达式，但很快就变得混乱。然后呈现了DOM解析器：稳定，但受限于浏览器。

像`node-html-parser`这样的库抽象化了这种痛苦。它们让您能够像使用jQuery一样查询HTML，但是在Node.js的服务器端。它快速，对脏HTML有容忍度，并且对DOM友好。

还有`jsdom`，模拟了一个完整的浏览器环境。它更重一些，但更彻底，为操作和交互创建了一个完整的文档对象模型（DOM）。

也不要忘记Cheerio。它将速度与类jQuery语法和更小的占用空间相结合，愉快地坐在两者之间。

## 另请参阅
如果你渴望了解更多，请深入这些资料：
- [DOM解析和序列化W3C规范](https://www.w3.org/TR/DOM-Parsing/)
- [node-html-parser的GitHub页面](https://github.com/taoqf/node-html-parser)
- [jsdom的GitHub仓库](https://github.com/jsdom/jsdom)
- [Cheerio网站](https://cheerio.js.org/)
