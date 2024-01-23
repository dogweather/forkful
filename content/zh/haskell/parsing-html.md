---
title:                "解析HTML"
date:                  2024-01-20T15:31:54.182026-07:00
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? 什么以及为什么？
解析HTML是指读取和转换HTML文档的内容，以便程序能理解和操作。程序员通常做这件事来抓取网页数据，自动化测试网站，或是处理网页内容。

## How to: 如何操作
使用Haskell解析HTML，我们可以用`hxt`库。这个例子简单展示了如何加载HTML，找到特定的数据。

```Haskell
import Text.XML.HXT.Core

main :: IO ()
main = do
  html <- readFile "example.html"
  let doc = readString [withParseHTML yes, withWarnings no] html
  links <- runX $ doc >>> css "a" ! "href"
  print links

example.html 的内容:
<html>
  <body>
    <a href="http://example.com">Example</a>
    <a href="http://haskell.org">Haskell</a>
  </body>
</html>

输出结果:
["http://example.com", "http://haskell.org"]
```
这个程序读取`example.html`文件，然后找到所有的`<a>`标签，并提取它们的`href`属性值。

## Deep Dive 深度剖析
Haskell处理HTML的历史比较短。它更知名于处理函数式编程模型。随着Web编程的普及，一些库像`hxt`出现了。而`pandoc`, Haskell 的文档转换工具，也可以处理HTML。

其他语言有更多的HTML解析选项：Python有`BeautifulSoup`和`lxml`，JavaScript有`cheerio`等。但是Haskell的解析库通常更注重类型安全和函数式程序设计原则。`hxt`利用了Haskell的强大类型系统和箭头(Arrow)抽象来处理XML/HTML文档。

实现时，`hxt`与其他解析库有所不同。它的核心是代数数据类型和箭头。数据类型代表文档的结构，而箭头则用于指导解析过程和抽取数据。

## See Also 另请参阅
- `hxt`库文档: http://hackage.haskell.org/package/hxt
- `pandoc`文档转换工具: https://pandoc.org
- Online tutorial on Haskell XML parsing: https://wiki.haskell.org/HXT 

请注意，链接可能会随时间变动，请查看最新的文档和资源。
