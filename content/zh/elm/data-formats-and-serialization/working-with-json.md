---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:52.886221-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elm\u5BF9JSON\u5904\u7406\u7684\u6001\
  \u5EA6\u662F\u660E\u786E\u548C\u5B89\u5168\u7684\uFF0C\u4E3B\u8981\u4F7F\u7528`Json.Decode`\u548C\
  `Json.Encode`\u6A21\u5757\u3002\u5F00\u59CB\u5904\u7406JSON\u4E4B\u524D\uFF0C\u4F60\
  \u9996\u5148\u9700\u8981\u4E3A\u4F60\u7684\u6570\u636E\u7C7B\u578B\u5B9A\u4E49\u4E00\
  \u4E2A\u89E3\u7801\u5668\u3002\u5047\u8BBE\u6211\u4EEC\u6B63\u5728\u5904\u7406\u4E00\
  \u4E2A\u7B80\u5355\u7684\u7528\u6237\u8D44\u6599\u5BF9\u8C61\u3002 \u9996\u5148\uFF0C\
  \u5B9A\u4E49\u4F60\u7684Elm\u7C7B\u578B\uFF1A."
lastmod: '2024-03-13T22:44:47.696046-06:00'
model: gpt-4-0125-preview
summary: "Elm\u5BF9JSON\u5904\u7406\u7684\u6001\u5EA6\u662F\u660E\u786E\u548C\u5B89\
  \u5168\u7684\uFF0C\u4E3B\u8981\u4F7F\u7528`Json.Decode`\u548C`Json.Encode`\u6A21\
  \u5757\u3002\u5F00\u59CB\u5904\u7406JSON\u4E4B\u524D\uFF0C\u4F60\u9996\u5148\u9700\
  \u8981\u4E3A\u4F60\u7684\u6570\u636E\u7C7B\u578B\u5B9A\u4E49\u4E00\u4E2A\u89E3\u7801\
  \u5668\u3002\u5047\u8BBE\u6211\u4EEC\u6B63\u5728\u5904\u7406\u4E00\u4E2A\u7B80\u5355\
  \u7684\u7528\u6237\u8D44\u6599\u5BF9\u8C61."
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
