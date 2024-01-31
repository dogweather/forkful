---
title:                "处理 YAML 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
## 什么是YAML编程，为什么要用？

Haskell中使用YAML常见于配置文件解析和数据交换。它易于阅读，支持复杂结构，能让程序更灵活。

## How to:
## 如何实现:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml

-- 假设我们有一个YAML文件：config.yaml
-- 内容如下：
-- name: "Zhang San"
-- age: 30
-- skills:
--   - Haskell
--   - Java

-- 定义对应YAML结构的Haskell类型
data Person = Person 
  { name :: String
  , age :: Int
  , skills :: [String]
  } deriving (Show)

instance FromJSON Person where
  parseJSON (Object v) =
    Person <$> v .: "name"
           <*> v .: "age"
           <*> v .: "skills"
  parseJSON _ = fail "Expected an Object for Person"

-- 解析YAML文件
main :: IO ()
main = do
  eitherPerson <- decodeFileEither "config.yaml" :: IO (Either ParseException Person)
  case eitherPerson of
    Left err -> putStrLn $ "Error parsing YAML file: " ++ show err
    Right person -> print person
```

输出样例：

```
Person {name = "Zhang San", age = 30, skills = ["Haskell","Java"]}
```

## Deep Dive:
## 深入探索:

YAML诞生于2001年，它比JSON更适合复杂环境。Haskell社区使用`yaml`包处理YAML格式。其他格式，如JSON和XML, 在某些场景更受欢迎。YAML在Haskell中和`aeson`包类似，利用了类型类（Typeclasses）来实现解析和生成YAML数据。

## See Also:
## 更多信息:

- Haskell `yaml` package: https://hackage.haskell.org/package/yaml
- YAML 官方网站: https://yaml.org
- Aeson: 对比YAML在Haskell中的JSON实现: https://hackage.haskell.org/package/aeson
