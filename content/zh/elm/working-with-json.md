---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? - 什么以及为什么?
JSON处理是让程序读写结构化数据的方式。程序员使用它因为JSON简洁，易读，且是Web应用数据交换的标准格式。

## How to: - 如何操作：
在Elm中处理JSON，主要通过`Json.Decode`和`Json.Encode`模块。

```Elm
module Main exposing (..)
import Html exposing (text)
import Json.Decode as Decode
import Json.Encode as Encode

type alias User =
    { name : String, age : Int }

-- 解码JSON字符串为Elm类型
userDecoder : Decode.Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "name" Decode.string)
        (Decode.field "age" Decode.int)

-- 示例JSON字符串
jsonString : String
jsonString = "{\"name\":\"Alice\",\"age\":30}"

-- 解码操作
decodedUser : Result Decode.Error User
decodedUser =
    Decode.decodeString userDecoder jsonString

-- 编码Elm类型为JSON字符串
userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ("name", Encode.string user.name)
        , ("age", Encode.int user.age)
        ]

-- 编码操作
encodedJson : String
encodedJson =
    userEncoder { name = "Bob", age = 25 }
        |> Encode.encode 0

-- 主函数显示结果
main =
    Html.div []
        [ case decodedUser of
            Ok user ->
                text ("Decoded: " ++ user.name ++ ", " ++ String.fromInt user.age)
            Err error ->
                text ("Error: " ++ Decode.errorToString error)
        , Html.br [] []
        , text ("Encoded: " ++ encodedJson)
        ]
```

Sample output:
```
Decoded: Alice, 30
Encoded: {"name":"Bob","age":25}
```

## Deep Dive - 深入了解
Elm对JSON的处理有一套类型安全的系统。不像JavaScript, Elm在运行前就确保了JSON结构的正确性；错误处理是编码的核心部分。

- 历史背景: Elm的JSON解码曾是个挑战，但现在`elm/json`库让它简单化。
- 替代品: 除了核心的`elm/json`库，社区也制造了一些辅助工具如`elm-graphql`。
- 执行细节: 解码器和编码器是函数，转换数据从而保证类型安全。

## See Also - 另请参阅
- Elm JSON guide: https://guide.elm-lang.org/effects/json.html
- `elm/json` package: https://package.elm-lang.org/packages/elm/json/latest/
- Elm decode examples: https://elm-lang.org/examples/json
