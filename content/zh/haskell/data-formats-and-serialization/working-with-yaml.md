---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:26.997217-07:00
description: "YAML\uFF0C\u5168\u79F0\u4E3A\u201CYAML Ain't Markup Language\u201D\uFF08\
  YAML\u4E0D\u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u5BF9\u4EBA\
  \u7C7B\u53CB\u597D\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u7528\
  \u4E8E\u6240\u6709\u7F16\u7A0B\u8BED\u8A00\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u5728\
  \u914D\u7F6E\u6587\u4EF6\u548C\u8BED\u8A00\u95F4\u7684\u6570\u636E\u4EA4\u6362\u4E2D\
  \u4F7F\u7528YAML\uFF0C\u56E0\u4E3A\u5B83\u7684\u53EF\u8BFB\u6027\u597D\u548C\u7ED3\
  \u6784\u7B80\u5355\u3002"
lastmod: '2024-03-13T22:44:47.838918-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5168\u79F0\u4E3A\u201CYAML Ain't Markup Language\u201D\uFF08\
  YAML\u4E0D\u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u5BF9\u4EBA\
  \u7C7B\u53CB\u597D\u7684\u6570\u636E\u5E8F\u5217\u5316\u6807\u51C6\uFF0C\u53EF\u7528\
  \u4E8E\u6240\u6709\u7F16\u7A0B\u8BED\u8A00\u3002\u7A0B\u5E8F\u5458\u901A\u5E38\u5728\
  \u914D\u7F6E\u6587\u4EF6\u548C\u8BED\u8A00\u95F4\u7684\u6570\u636E\u4EA4\u6362\u4E2D\
  \u4F7F\u7528YAML\uFF0C\u56E0\u4E3A\u5B83\u7684\u53EF\u8BFB\u6027\u597D\u548C\u7ED3\
  \u6784\u7B80\u5355\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么和为什么？

YAML，全称为“YAML Ain't Markup Language”（YAML不是标记语言），是一种对人类友好的数据序列化标准，可用于所有编程语言。程序员通常在配置文件和语言间的数据交换中使用YAML，因为它的可读性好和结构简单。

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
