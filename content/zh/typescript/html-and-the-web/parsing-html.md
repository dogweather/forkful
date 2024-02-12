---
title:                "解析HTML"
aliases: - /zh/typescript/parsing-html.md
date:                  2024-02-03T19:13:17.060490-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么?

解析HTML意味着仔细检查HTML代码，以查找、提取或操作信息。程序员这样做是为了与网页内容互动——可能是爬取数据，或自动化浏览器。

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
