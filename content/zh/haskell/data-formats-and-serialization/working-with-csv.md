---
aliases:
- /zh/haskell/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:54.926806-07:00
description: "\u5904\u7406CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\
  \u6D89\u53CA\u5230\u89E3\u6790\u548C\u751F\u6210\u5B58\u50A8\u4EE5\u7B80\u5355\u6587\
  \u672C\u683C\u5F0F\u5448\u73B0\u7684\u8868\u683C\u6570\u636E\u7684\u6587\u4EF6\u3002\
  \u7A0B\u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u8FD9\u4E00\u4EFB\u52A1\uFF0C\u4EE5\u9AD8\
  \u6548\u5730\u4ECE\u7535\u5B50\u8868\u683C\u3001\u6570\u636E\u5E93\u4E2D\u5BFC\u5165\
  \u6216\u5BFC\u51FA\u6570\u636E\uFF0C\u6216\u4FC3\u8FDB\u4E0D\u540C\u7A0B\u5E8F\u4E4B\
  \u95F4\u7684\u6570\u636E\u4EA4\u6362\u3002"
lastmod: 2024-02-18 23:08:59.198388
model: gpt-4-0125-preview
summary: "\u5904\u7406CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6587\u4EF6\u6D89\
  \u53CA\u5230\u89E3\u6790\u548C\u751F\u6210\u5B58\u50A8\u4EE5\u7B80\u5355\u6587\u672C\
  \u683C\u5F0F\u5448\u73B0\u7684\u8868\u683C\u6570\u636E\u7684\u6587\u4EF6\u3002\u7A0B\
  \u5E8F\u5458\u7ECF\u5E38\u8FDB\u884C\u8FD9\u4E00\u4EFB\u52A1\uFF0C\u4EE5\u9AD8\u6548\
  \u5730\u4ECE\u7535\u5B50\u8868\u683C\u3001\u6570\u636E\u5E93\u4E2D\u5BFC\u5165\u6216\
  \u5BFC\u51FA\u6570\u636E\uFF0C\u6216\u4FC3\u8FDB\u4E0D\u540C\u7A0B\u5E8F\u4E4B\u95F4\
  \u7684\u6570\u636E\u4EA4\u6362\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 是什么 & 为什么？

处理CSV（逗号分隔值）文件涉及到解析和生成存储以简单文本格式呈现的表格数据的文件。程序员经常进行这一任务，以高效地从电子表格、数据库中导入或导出数据，或促进不同程序之间的数据交换。

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
