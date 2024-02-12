---
title:                "处理XML"
aliases:
- /zh/elm/working-with-xml.md
date:                  2024-01-26T04:30:55.749216-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-xml.md"
---

{{< edit_this_page >}}

## 什么和为什么？
处理 XML 意味着在 Elm 中解析、转换和生成 XML 文档。这样做是为了与许多使用 XML 作为数据格式的网络服务和遗留系统进行交互。

## 如何操作：
在 Elm 中，你可以通过 `elm/xml` 包来处理 XML。这里快速查看解析一个 XML 片段的方法：

```Elm
import Xml.Decode exposing (..)
import Xml.Decode.Pipeline exposing (..)

xmlString = """
<book id="123">
    <title>Elm in Action</title>
    <author>Robin Heggelund Hansen</author>
</book>
"""

type alias Book =
    { id : String
    , title : String
    , author : String
    }

bookDecoder : Decoder Book
bookDecoder =
    decode Book
        |> required "id" (attribute "id")
        |> required "title" (child "title" (content text))
        |> required "author" (child "author" (content text))

case Xml.Decode.fromString bookDecoder xmlString of
    Ok book ->
        -- 在这里处理解码后的书籍
        Debug.toString book

    Err error ->
        -- 处理错误
        Debug.toString error
```

假设没有错误的示例输出：

```Elm
"{ id = \"123\", title = \"Elm in Action\", author = \"Robin Heggelund Hansen\" }"
```

## 深入探讨
XML（可扩展标记语言）自 90 年代末就已出现，那时网络上文字内容繁多，对结构化的同时又灵活的数据传输方式需求极为迫切。由于冗长和复杂，XML 逐渐地对 JSON 失去了一些地位。然而，在企业环境或诸如 SOAP 这类协议中，XML 仍然很常见。

Elm 对 XML 的处理方式是函数式且类型安全的。使用 `elm/xml` 包意味着接受 Elm 的显性和可靠性哲学。在解析方面，该包提供了一系列的解码器，您可以组合这些解码器来处理 XML 结构。

与 JavaScript 的 DOMParser 或 Python 的 ElementTree 等替代方法相比，Elm 的方法可能看起来更冗长，但确保了安全性。没有运行时异常，如缺失字段或类型不匹配；如果出现问题，您会在编译时收到错误提示。

`elm/xml` 解码函数的核心是将 XML 节点映射到 Elm 类型。您构建的解码器会反映您的数据结构，确保您的 Elm 应用程序像处理其内部数据结构一样严格地处理 XML。

在 Elm 中生成 XML 较为少见，但可以通过 `elm/xml` 的对应部分 `Xml.Encode` 来实现。

## 参见
- Elm 关于 JSON 的指南，也适用于 XML 思维: [https://guide.elm-lang.org/interop/json.html](https://guide.elm-lang.org/interop/json.html)
- 为了更深入了解 XML 本身，参考 W3C 的 XML 标准: [https://www.w3.org/XML/](https://www.w3.org/XML/)
