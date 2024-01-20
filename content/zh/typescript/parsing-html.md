---
title:                "解析HTML"
html_title:           "Clojure: 解析HTML"
simple_title:         "解析HTML"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## 什么&为什么？

解析HTML指的是使用程序从HTML文档中提取信息。程序员解析HTML以从静态网页获取数据，或者操控和修改网页内容。

## 如何：

你可以使用TypeScript和一个库，例如htmlparser2来解析HTML。举个例子：

```TypeScript
import { Parser } from "htmlparser2";
const html = "<html><title>Hello World</title></html>";
const parser = new Parser({
  onopentag(name, attribs){
    if(name === "title"){
      console.log("Title found!");
    }
  },
  ontext(text){
    console.log("-->", text);
  }
});

parser.write(html);
parser.end();
```

运行上述代码，输出会是这样：

```TypeScript
Title found!
--> Hello World
```

## 深入探讨

事实上，早期的网页浏览器为了显示HTML 就需要解析它。现在，它也被用于Web爬虫，这些程序会搜索Web，收集信息。

一种替代的HTML解析方案是使用JSoup，这是一个用于Java平台的库。然而，TypeScript/JavaScript因为它的异步性质和强大的字符串处理能力，成为了HTML解析的标准之一。

解析HTML的过程包括两个主要阶段：词法分析（转换输入文本为令牌）和语法分析（将这些令牌转换为更结构化的形式）。

## 另请参阅：

1. [htmlparser2的GitHub仓库](https://github.com/fb55/htmlparser2)
3. [JSoup的主页](https://jsoup.org/)