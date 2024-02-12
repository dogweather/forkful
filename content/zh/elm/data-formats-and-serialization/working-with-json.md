---
title:                "使用JSON进行编程"
aliases:
- /zh/elm/working-with-json/
date:                  2024-02-03T19:22:52.886221-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用JSON进行编程"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在Elm中处理JSON关乎于将JSON数据解码为Elm类型以及将Elm值编码回JSON。这一过程对于网络应用程序与API和外部数据源交互至关重要，允许在客户端（Elm）与服务器或其他服务之间无缝地交换数据。

## 如何操作：

Elm对JSON处理的态度是明确和安全的，主要使用`Json.Decode`和`Json.Encode`模块。开始处理JSON之前，你首先需要为你的数据类型定义一个解码器。假设我们正在处理一个简单的用户资料对象。

首先，定义你的Elm类型：

```elm
type alias UserProfile = 
    { id : Int
    , name : String
    , email : String
    }
```

### 将JSON解码为Elm

要将JSON字符串解码为`UserProfile`类型，创建一个解码器：

```elm
import Json.Decode exposing (Decoder, int, string, field, map3)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    map3 UserProfile
        (field "id" int)
        (field "name" string)
        (field "email" string)
```

解码一个JSON对象：

```elm
import Json.Decode exposing (decodeString)

jsonString : String
jsonString = 
    """{"id": 1, "name": "John Doe", "email": "john@example.com"}"""

decoded : Result String UserProfile
decoded =
    decodeString userProfileDecoder jsonString

{- 示例输出:
Result.Ok { id = 1, name = "John Doe", email = "john@example.com" }
-}
```

### 将Elm编码为JSON

要将Elm值编码回JSON，利用`Json.Encode`模块。

```elm
import Json.Encode exposing (object, int, string)

encodeUserProfile : UserProfile -> String
encodeUserProfile userProfile =
    object
        [ ("id", int userProfile.id)
        , ("name", string userProfile.name)
        , ("email", string userProfile.email)
        ]
        |> Json.Encode.encode 0

{-
使用方式：
encodeUserProfile { id = 1, name = "John Doe", email = "john@example.com" }

示例输出：
"{"id":1,"name":"John Doe","email":"john@example.com"}"
-}
```

### 第三方库

像`elm-json-decode-pipeline`这样的Elm包可以通过管道风格简化解码器的创建，这对解码复杂对象特别有用。

首先，将库添加到您的项目中：

```shell
elm install NoRedInk/elm-json-decode-pipeline
```

然后，您可以如下简化解码器的定义：

```elm
import Json.Decode exposing (int, string, succeed)
import Json.Decode.Pipeline exposing (required, decode)

userProfileDecoder : Decoder UserProfile
userProfileDecoder =
    decode UserProfile
        |> required "id" int
        |> required "name" string
        |> required "email" string

{- 如之前使用decodeString来解码JSON字符串时一样使用这个解码器。-}
```

这种方法简化了解码器，使代码更清晰、易于维护，特别是当数据结构变得更加复杂时。
