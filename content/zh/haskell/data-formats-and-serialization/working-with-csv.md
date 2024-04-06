---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:54.926806-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Haskell\u4E2D\uFF0C\u53EF\u4EE5\
  \u4F7F\u7528`cassava`\u5E93\u6765\u5904\u7406CSV\u6587\u4EF6\uFF0C\u8FD9\u662F\u7528\
  \u4E8E\u6B64\u76EE\u7684\u7684\u6D41\u884C\u7684\u7B2C\u4E09\u65B9\u5E93\u4E4B\u4E00\
  \u3002\u4EE5\u4E0B\u662F\u4F7F\u7528`cassava`\u8BFB\u53D6\u548C\u5199\u5165CSV\u6587\
  \u4EF6\u7684\u793A\u4F8B\u3002 **1. \u8BFB\u53D6CSV\u6587\u4EF6\uFF1A** \u9996\u5148\
  \uFF0C\u786E\u4FDD\u901A\u8FC7\u5C06\u5176\u6DFB\u52A0\u5230\u9879\u76EE\u7684cabal\u6587\
  \u4EF6\u6216\u4F7F\u7528Stack\u6765\u5B89\u88C5`cassava`\u3002\u2026"
lastmod: '2024-04-05T22:38:47.002124-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Haskell\u4E2D\uFF0C\u53EF\u4EE5\u4F7F\
  \u7528`cassava`\u5E93\u6765\u5904\u7406CSV\u6587\u4EF6\uFF0C\u8FD9\u662F\u7528\u4E8E\
  \u6B64\u76EE\u7684\u7684\u6D41\u884C\u7684\u7B2C\u4E09\u65B9\u5E93\u4E4B\u4E00\u3002\
  \u4EE5\u4E0B\u662F\u4F7F\u7528`cassava`\u8BFB\u53D6\u548C\u5199\u5165CSV\u6587\u4EF6\
  \u7684\u793A\u4F8B\u3002 **1. \u8BFB\u53D6CSV\u6587\u4EF6\uFF1A** \u9996\u5148\uFF0C\
  \u786E\u4FDD\u901A\u8FC7\u5C06\u5176\u6DFB\u52A0\u5230\u9879\u76EE\u7684cabal\u6587\
  \u4EF6\u6216\u4F7F\u7528Stack\u6765\u5B89\u88C5`cassava`\u3002 \u8FD9\u91CC\u6709\
  \u4E00\u4E2A\u7B80\u5355\u7684\u793A\u4F8B\uFF0C\u7528\u4E8E\u8BFB\u53D6CSV\u6587\
  \u4EF6\u5E76\u6253\u5370\u6BCF\u6761\u8BB0\u5F55\u3002\u6211\u4EEC\u5047\u8BBECSV\u6587\
  \u4EF6\u6709\u4E24\u5217\uFF1A\u540D\u79F0\u548C\u5E74\u9F84\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 如何操作：
在Haskell中，可以使用`cassava`库来处理CSV文件，这是用于此目的的流行的第三方库之一。以下是使用`cassava`读取和写入CSV文件的示例。

**1. 读取CSV文件：**

首先，确保通过将其添加到项目的cabal文件或使用Stack来安装`cassava`。

这里有一个简单的示例，用于读取CSV文件并打印每条记录。我们假设CSV文件有两列：名称和年龄。

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " is " ++ show (age :: Int) ++ " years old."
```

假设`people.csv`包含：
```
John,30
Jane,25
```
输出将会是：
```
John is 30 years old.
Jane is 25 years old.
```

**2. 写入CSV文件：**

要创建CSV文件，你可以使用`cassava`的`encode`函数。

以下是如何将记录列表写入CSV文件的方法：

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

运行此程序后，`output.csv`将包含：

```
John,30
Jane,25
```

这个对使用`cassava`库在Haskell中处理CSV文件的简介展示了如何读取和写入CSV文件，使得数据操作任务对于那些对该语言不熟悉的人更加易于接近。
