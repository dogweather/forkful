---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (是什么与为什么？)
在 Haskell 中处理 JSON 主要是为了数据交互。JSON，轻便、易读，广泛应用于网络通信和配置文件。

## How to: (如何操作：)
```Haskell
-- 需要 Aeson 依赖
import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- 假定有如下 JSON 字符串：
jsonStr = "{\"name\": \"Xiao Ming\", \"age\": 25}"

-- 解析 JSON
-- 'eitherDecode' : 字符串 -> Either String a
-- 使用时宜定义类型
data Person = Person {
    name :: String,
    age :: Int
} deriving (Show, Eq, FromJSON)

main = do
    let result = eitherDecode jsonStr :: Either String Person
    case result of
        Left err -> putStrLn err
        Right p -> print p

-- 输出示范：
-- Person {name = "Xiao Ming", age = 25}
```

## Deep Dive (深入探讨)
- 历史背景：JSON 诞生于 2000 年代初，由 Douglas Crockford 推广，应对 XML 的繁复。
- 其他选项: XML, YAML 等格式也用于数据交换，但 JSON 更适合 JS 世界。
- 实现细节：在 Haskell 中，Aeson 库利用类型类 (`FromJSON` 和 `ToJSON`) 自动实现编解码。

## See Also (另请参阅)
- [Hackage 的 Aeson 页面](https://hackage.haskell.org/package/aeson)
- [JSON 官方网站](http://json.org/)