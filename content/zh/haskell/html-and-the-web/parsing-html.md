---
title:                "解析HTML"
aliases:
- /zh/haskell/parsing-html.md
date:                  2024-02-03T19:12:20.469295-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Haskell 中解析 HTML 允许你提取数据、操作 HTML 内容或以编程方式与网页交互。这种操作对于网络爬虫、自动化测试网页应用、以及从网站挖掘数据等任务至关重要 - 利用 Haskell 强大的类型系统和函数式编程范例来确保代码的健壮性和简洁性。

## 如何进行：

要在 Haskell 中解析 HTML，我们将使用 `tagsoup` 库，因其简单和灵活性而选用。首先，通过在项目的 cabal 文件中添加 `tagsoup` 或运行 `cabal install tagsoup` 来确保安装该库。

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup

-- 示例 HTML 以作演示
let sampleHtml = "<html><body><p>Learn Haskell!</p><a href='http://example.com'>点击这里</a></body></html>"

-- 解析 HTML 并过滤链接 (a 标签)
let tags = parseTags sampleHtml
let links = [fromAttrib "href" tag | tag <- tags, isTagOpenName "a" tag]

-- 打印提取的链接
print links
```

示例输出：
```plaintext
["http://example.com"]
```

对于更复杂的 HTML 解析需求，可以考虑使用 `pandoc` 库，特别是当你处理文档转换时。它异常灵活，但带有更多的复杂性：

```haskell
import Text.Pandoc

-- 假设你已经加载了一个 Pandoc 文档（doc），例如，通过读取一个文件
let doc = ... -- 你的 Pandoc 文档在这里

-- 将文档转换为 HTML 字符串
let htmlString = writeHtmlString def doc

-- 现在，你会像上面那样解析 `htmlString` 或根据你的需求进行处理。
```
请记住，`pandoc` 是一个更大的库，专注于多种标记格式之间的转换，因此如果你需要这些额外的能力，或者你在应用中已经在处理文档格式，就应该使用它。
