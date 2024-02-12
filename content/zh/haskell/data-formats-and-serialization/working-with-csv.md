---
title:                "处理CSV文件"
aliases:
- /zh/haskell/working-with-csv/
date:                  2024-02-03T19:19:54.926806-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
