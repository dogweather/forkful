---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:58.571375-07:00
description: "\u5728 Haskell \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u6D89\u53CA\u5230\u5C06 JSON \u6570\u636E\u89E3\u6790\u6210 Haskell\
  \ \u7C7B\u578B\u4EE5\u53CA\u5C06 Haskell \u7C7B\u578B\u53CD\u5411\u8F6C\u6362\u6210\
  \ JSON\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4ED6\u4EEC\
  \u7684 Haskell \u5E94\u7528\u80FD\u591F\u4E0E\u7F51\u7EDC\u670D\u52A1\u6216 API\
  \ \u65E0\u7F1D\u4EA4\u6362\u6570\u636E\uFF0C\u8FD9\u5728\u73B0\u4EE3\u8F6F\u4EF6\
  \u5F00\u53D1\u4E2D\u662F\u8DE8\u5E73\u53F0\u6570\u636E\u4EA4\u6362\u7684\u5E38\u89C1\
  \u505A\u6CD5\u3002"
lastmod: '2024-03-13T22:44:47.840298-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Haskell \u4E2D\u5904\u7406 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\
  \u793A\u6CD5\uFF09\u6D89\u53CA\u5230\u5C06 JSON \u6570\u636E\u89E3\u6790\u6210 Haskell\
  \ \u7C7B\u578B\u4EE5\u53CA\u5C06 Haskell \u7C7B\u578B\u53CD\u5411\u8F6C\u6362\u6210\
  \ JSON\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u4F7F\u4ED6\u4EEC\
  \u7684 Haskell \u5E94\u7528\u80FD\u591F\u4E0E\u7F51\u7EDC\u670D\u52A1\u6216 API\
  \ \u65E0\u7F1D\u4EA4\u6362\u6570\u636E\uFF0C\u8FD9\u5728\u73B0\u4EE3\u8F6F\u4EF6\
  \u5F00\u53D1\u4E2D\u662F\u8DE8\u5E73\u53F0\u6570\u636E\u4EA4\u6362\u7684\u5E38\u89C1\
  \u505A\u6CD5\u3002."
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

## 如何操作：
Haskell 并没有像 JavaScript 那样内置对 JSON 的支持，但借助于第三方库比如 **Aeson**，处理 JSON 就变得简单了。Aeson 提供了用于编码（将 Haskell 值转换为 JSON）和解码（将 JSON 解析成 Haskell 值）的高级和低级函数。

### 安装 Aeson
首先，通过更新你的 `.cabal` 文件或直接使用 Stack 或 Cabal 来将 Aeson 添加到你项目的依赖中：

```shell
cabal update && cabal install aeson
```
或者，如果你是使用 Stack：
```shell
stack install aeson
```

### 解析 JSON
我们从一个基本示例开始，把 JSON 数据解码成 Haskell 类型。假设我们有以下代表一个人的 JSON：

```json
{
  "name": "John Doe",
  "age": 30
}
```

首先，定义一个相应的 Haskell 数据类型并使其成为 `FromJSON` 的一个实例：

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- 一个从文件解码 JSON 的函数
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
用法：
假设 `person.json` 包含上面显示的 JSON 数据，运行：
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
示例输出：
```haskell
Just (Person {name = "John Doe", age = 30})
```

### 将 Haskell 值编码为 JSON
要将一个 Haskell 值转换回 JSON，你需要使你的类型成为 `ToJSON` 的一个实例，然后使用 `encode`。

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- 假设前面的 Person 类型

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
示例输出：
```json
{"name":"Jane Doe","age":32}
```

这些例子展示了使用 Aeson 在 Haskell 中处理 JSON 的基础。请记住，Aeson 提供了更多功能，包括自定义解析规则、处理复杂的嵌套 JSON 等，适合各种需要和场景。
