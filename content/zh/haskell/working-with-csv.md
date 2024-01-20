---
title:                "处理 CSV 文件"
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
CSV，即逗号分隔值，是存储表格数据的简单格式。程序员用它因为它跨平台、可读性好、易于手工编辑。

## How to: (如何操作：)
在Haskell中处理CSV，可以用`cassava`库。下面是如何读取和写入CSV文件：

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- 定义一个简单的类型来匹配CSV中的数据
type Name = String
type Age = Int
type Person = (Name, Age)

-- 从CSV文件读取数据
readCsv :: FilePath -> IO (Either String (V.Vector Person))
readCsv filePath = do
    csvData <- BL.readFile filePath
    return $ decode NoHeader csvData

-- 写数据到CSV文件
writeCsv :: FilePath -> V.Vector Person -> IO ()
writeCsv filePath people = BL.writeFile filePath (encode people)

-- 示例输出
main :: IO ()
main = do
  let people = V.fromList [("Alice", 30), ("Bob", 35)]
  writeCsv "people.csv" people
  
  result <- readCsv "people.csv"
  case result of
    Left err -> putStrLn err
    Right v -> V.forM_ v $ \(name, age) ->
      putStrLn $ name ++ " is " ++ show age ++ " years old."
```

## Deep Dive (深入探讨)
CSV格式有历史悠久，70年代就开始使用。它的替代方案有JSON、XML等，这些格式更结构化但不如CSV轻量。在Haskell中，`cassava`库利用了惰性IO与向量处理进行高效解析与生成。

## See Also (另请参阅)
- `cassava`库文档：[http://hackage.haskell.org/package/cassava](http://hackage.haskell.org/package/cassava)
- Haskell官方教程：[https://www.haskell.org/documentation/](https://www.haskell.org/documentation/)
- CSV标准：[https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)