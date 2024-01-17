---
title:                "使用yaml编程"
html_title:           "Elm: 使用yaml编程"
simple_title:         "使用yaml编程"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

# Elm 中的YAML：什么 & 为什么要使用

YAML是一种轻量级的数据序列化语言，用于在不同编程环境下存储和传输数据。程序员们使用YAML是为了在不同系统之间共享数据，或者将数据存储为可读性较高的格式，便于维护和管理。

# 如何使用

```elm
import Json.Decode as Decode
import Yaml.Decode as Yaml

yamlDecoder : Decode.Decoder a -> String -> Decode.Result a
yamlDecoder decoder string =
    Yaml.decode string
        |> Decode.andThen decoder

decoder : Decode.Decoder MyData
decoder =
    Decode.succeed MyData
        |> Decode.field "key" Decode.int
        |> Decode.field "value" Decode.string

yamlString : String
yamlString =
    """
    key: 123
    value: "Hello World!"
    """

result : Decode.Result MyData
result =
    yamlDecoder decoder yamlString
```

输出：`Ok { key = 123, value = "Hello World!" }`

# 深入解析

- 历史背景：YAML最初于2001年由Clark Evans创建，旨在提供一种易于人类阅读和机器解析的数据格式。
- 替代方案：除了YAML，JSON也是一种流行的数据序列化语言。YAML相比JSON更易读，并支持注释和锚点功能。
- 实现细节：Elm中使用YAML需要通过第三方库，如上例所示，需要使用`Json.Decode`和`Yaml.Decode`模块。

# 查看更多

- [Official YAML Website](https://yaml.org/)
- [Elm Yaml Package](https://package.elm-lang.org/packages/BrianHicks/elm-yaml/latest/)