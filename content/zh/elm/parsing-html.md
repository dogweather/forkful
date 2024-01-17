---
title:                "解析HTML"
html_title:           "Elm: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/parsing-html.md"
---

{{< edit_this_page >}}

## 什么和为什么？
解析HTML是将网页文档转换成可读取的数据结构的过程。程序员通常会这样做，以便能够从网页文档中提取所需的信息，并对其进行处理。

## 如何：
```Elm
import Html.Parser exposing (..)

-- 提供HTML文档
htmlDoc = "
<!DOCTYPE html>
<html>
<head>
  <title>Elm Article</title>
</head>
<body>
  <h1>Hello, world!</h1>
</body>
</html>
"

-- 解析HTML
parsedDoc = parse htmlDoc

-- 输出结果
parsedDoc
-- <title>Elm Article</title><h1>Hello, world!</h1>

```

## 深入探讨：
解析HTML的历史可以追溯到早期的网络浏览器，它们需要解析HTML文档来显示网页内容。除了Elm，还有其他语言也能解析HTML，例如JavaScript中的DOM操作。而在Elm中，可以使用Html.Parser来解析HTML文档，它提供了许多有用的函数来帮助我们处理HTML文档中的元素和属性。

## 参考资料：
- [Elm Html模块文档](https://package.elm-lang.org/packages/elm/html/latest/)
- [HTML解析器的历史](https://en.wikipedia.org/wiki/HTML#History)
- [使用JavaScript解析HTML](https://www.w3schools.com/js/js_htmldom.asp)