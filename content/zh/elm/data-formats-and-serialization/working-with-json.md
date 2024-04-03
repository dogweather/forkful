---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:52.886221-07:00
description: "\u5728Elm\u4E2D\u5904\u7406JSON\u5173\u4E4E\u4E8E\u5C06JSON\u6570\u636E\
  \u89E3\u7801\u4E3AElm\u7C7B\u578B\u4EE5\u53CA\u5C06Elm\u503C\u7F16\u7801\u56DEJSON\u3002\
  \u8FD9\u4E00\u8FC7\u7A0B\u5BF9\u4E8E\u7F51\u7EDC\u5E94\u7528\u7A0B\u5E8F\u4E0EAPI\u548C\
  \u5916\u90E8\u6570\u636E\u6E90\u4EA4\u4E92\u81F3\u5173\u91CD\u8981\uFF0C\u5141\u8BB8\
  \u5728\u5BA2\u6237\u7AEF\uFF08Elm\uFF09\u4E0E\u670D\u52A1\u5668\u6216\u5176\u4ED6\
  \u670D\u52A1\u4E4B\u95F4\u65E0\u7F1D\u5730\u4EA4\u6362\u6570\u636E\u3002"
lastmod: '2024-03-13T22:44:47.696046-06:00'
model: gpt-4-0125-preview
summary: "\u5728Elm\u4E2D\u5904\u7406JSON\u5173\u4E4E\u4E8E\u5C06JSON\u6570\u636E\u89E3\
  \u7801\u4E3AElm\u7C7B\u578B\u4EE5\u53CA\u5C06Elm\u503C\u7F16\u7801\u56DEJSON\u3002\
  \u8FD9\u4E00\u8FC7\u7A0B\u5BF9\u4E8E\u7F51\u7EDC\u5E94\u7528\u7A0B\u5E8F\u4E0EAPI\u548C\
  \u5916\u90E8\u6570\u636E\u6E90\u4EA4\u4E92\u81F3\u5173\u91CD\u8981\uFF0C\u5141\u8BB8\
  \u5728\u5BA2\u6237\u7AEF\uFF08Elm\uFF09\u4E0E\u670D\u52A1\u5668\u6216\u5176\u4ED6\
  \u670D\u52A1\u4E4B\u95F4\u65E0\u7F1D\u5730\u4EA4\u6362\u6570\u636E\u3002."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
