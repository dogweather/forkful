---
title:                "处理XML"
aliases:
- /zh/haskell/working-with-xml.md
date:                  2024-01-26T04:32:00.941532-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Haskell 中处理 XML 涉及解析、操作和生成 XML 结构。程序员处理 XML 是为了与使用 XML 作为数据格式的众多应用程序和协议进行交互，例如 Web 服务和配置文件。

## 如何操作：

Haskell 提供了如 `xml-conduit` 这样的库来处理 XML。以下示例演示了解析 XML 字符串和查询元素的方法：

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

示例输出：

```
["World!"]
```

## 深入探索

XML，全称为可扩展标记语言，长期以来一直是数据序列化的基石，早在 JSON 崛起之前就已如此。它虽然冗长，但结构严格且标准化，非常适合于严格的企业环境、遗留系统，以及金融和医疗等行业。

Haskell 有几个用于 XML 的库；然而，`xml-conduit` 是最强大和广泛使用的一个，主要是因为它高效的流处理和解析能力，是处理数据流的 `conduit` 家族的一部分。

其他选择包括使用箭头进行解析和转换的 `HXT`（Haskell XML 工具箱），为 XML 操作提供了不同的范式。虽然现在 `HXT` 的受欢迎程度较低，主要是因为它的学习曲线较陡，但它仍然是某些用例的坚实选择。

在 Haskell 中实现 XML 处理时，必须注意编码问题，因为 Haskell 字符串是 Unicode 的，而 XML 数据可能不是。此外，XML 命名空间可能会增加解析的复杂性。

## 另见：

- `xml-conduit` 包文档：https://hackage.haskell.org/package/xml-conduit
- Haskell XML 工具箱（HXT）：http://hackage.haskell.org/package/hxt
- 《实际 Haskell 编程》一书，第 16 章，关于 XML 处理：http://book.realworldhaskell.org/read/xml.html
- Haskell Wiki 上的 XML：https://wiki.haskell.org/XML
