---
title:                "使用TOML"
aliases:
- zh/elm/working-with-toml.md
date:                  2024-01-26T04:21:22.590443-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
TOML，代表Tom的明显、最小化语言，是一种数据序列化语言。Elm程序员使用它来管理配置数据，因为它易于人类读写，且能整洁地映射到应用程序所需的键值对。

## 如何操作：
Elm没有内置的TOML解析器，但你可以与JavaScript互操作或使用社区包。下面是通过一个假想的`elm-toml`包解析TOML的方法：

```elm
import Toml

configToml : String
configToml =
    """
    [server]
    port = 8080
    """

parseResult : Result Toml.Decode.Error Toml.Value
parseResult =
    Toml.decodeString configToml
```

对于解码特定值：

```elm
portDecoder : Toml.Decode.Decoder Int
portDecoder =
    Toml.Decode.field "server" (Toml.Decode.field "port" Toml.Decode.int)

port : Result String Int
port =
    Toml.decodeString portDecoder configToml
```

如果解码成功，`port`的示例输出可能是`Ok 8080`。

## 深入探讨
TOML由GitHub的联合创始人Tom Preston-Werner创建，作为配置文件的简单语言。它与YAML和JSON竞争；TOML的语法旨在兼具两者的优点，重点是易于人类读写。

在Elm中，处理TOML通常需要通过JavaScript互操作，这可能有点麻烦。幸运的是，Elm社区资源丰富，存在几个第三方包。假想的`elm-toml`包可能会使用Elm的`Port`与JavaScript的TOML解析器对话，或直接在Elm中实现解析。

Elm的主要障碍是它静态类型化一切，所以你需要编写自定义解码器来处理TOML中的不同数据结构，这可能有点冗长但增加了安全性。

## 另见
有关TOML本身的规范和更多信息，请查看[TOML](https://toml.io)。
如果你正在寻找Elm和JavaScript互操作的实践方法，请从官方指南开始：[Elm Ports](https://guide.elm-lang.org/interop/ports.html)。
浏览[Elm Packages](https://package.elm-lang.org/)了解社区包或贡献。
