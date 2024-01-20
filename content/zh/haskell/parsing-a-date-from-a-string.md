---
title:                "从字符串解析日期"
date:                  2024-01-20T15:36:47.156761-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
解析字符串中的日期意味着将文本格式转换为日期类型。编程中这么做便于存储、排序和比较日期数据。

## How to: (如何操作：)
```Haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- 解析日期字符串为时间
parseDate :: String -> IO (Maybe UTCTime)
parseDate str = parseTimeM True defaultTimeLocale "%Y-%m-%d" str

main :: IO ()
main = do
  let dateString = "2023-03-25"
  parsedDate <- parseDate dateString
  case parsedDate of
    Just date -> putStrLn $ "Parsed date: " ++ show date
    Nothing -> putStrLn $ "Failed to parse date."

-- 示例输出
-- Parsed date: 2023-03-25 00:00:00 UTC
```

## Deep Dive (深入了解)
日期解析是 Haskell 历史中不断改进的过程。早期的探索包含了不同的库，比如`old-time`和`time`. 现在，标准方法通常使用`time`库。

解析字符串至日期也有替代方法，如使用`read`函数或第三方库比如`Thyme`和`Chronos`。实现细节上，Haskell 的强类型系统意味着解析必须完全匹配日期格式，否则会失败。

## See Also (另请参阅)
- `Data.Time.Format`的官方文档: [https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Format.html](https://hackage.haskell.org/package/time-1.12/docs/Data-Time-Format.html)
- `time`库官方概览: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)