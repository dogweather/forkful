---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:26.997217-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Haskell \u6CA1\u6709\u5185\u7F6E\u5BF9\
  \ YAML \u5904\u7406\u7684\u652F\u6301\uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u7B2C\
  \u4E09\u65B9\u5E93\uFF0C\u5982 `yaml` \u548C `aeson`\uFF0C\u6765\u89E3\u6790\u548C\
  \u751F\u6210 YAML \u6570\u636E\u3002\u4EE5\u4E0B\u662F\u4F60\u53EF\u4EE5\u5F00\u59CB\
  \u7684\u65B9\u6CD5\uFF1A #."
lastmod: '2024-03-13T22:44:47.838918-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u6CA1\u6709\u5185\u7F6E\u5BF9 YAML \u5904\u7406\u7684\u652F\u6301\
  \uFF0C\u4F46\u4F60\u53EF\u4EE5\u4F7F\u7528\u7B2C\u4E09\u65B9\u5E93\uFF0C\u5982 `yaml`\
  \ \u548C `aeson`\uFF0C\u6765\u89E3\u6790\u548C\u751F\u6210 YAML \u6570\u636E\u3002\
  \u4EE5\u4E0B\u662F\u4F60\u53EF\u4EE5\u5F00\u59CB\u7684\u65B9\u6CD5\uFF1A\n\n#."
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
Haskell 没有内置对 YAML 处理的支持，但你可以使用第三方库，如 `yaml` 和 `aeson`，来解析和生成 YAML 数据。以下是你可以开始的方法：

### 读取 YAML
首先，将 `yaml` 包添加到项目的依赖中。然后，你可以使用下面的例子来解析一个简单的 YAML 文档：

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- 示例 YAML 数据
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- 定义一个与 YAML 文档匹配的数据结构
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "解析 YAML 时出错：" ++ show err
    Right person -> print person
```
上述代码的示例输出可能看起来像：
```
Person {name = "John Doe", age = 30}
```

### 写入 YAML
要从 Haskell 数据结构生成 YAML，你可以使用 `yaml` 包的编码功能，如下所示：

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- 使用前一个例子中的 Person 数据结构

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
这个程序的输出将是一个 YAML 格式化的字符串：
```
name: Jane Doe
age: 25
```

这些例子应该作为使用 Haskell 处理 YAML 的起点。根据你的需要，你可能想探索这些库提供的更高级的功能和选项。
