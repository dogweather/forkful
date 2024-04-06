---
date: 2024-01-20 17:56:17.532286-07:00
description: "How to: \u600E\u9EBC\u505A \u5728Haskell\u4E2D\uFF0C`System.Environment`\u6A21\
  \u584A\u63D0\u4F9B\u8B80\u53D6\u547D\u4EE4\u5217\u53C3\u6578\u7684\u529F\u80FD\u3002\
  \u770B\u4E00\u4E0B\u600E\u9EBC\u7528\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.143879-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u9EBC\u505A \u5728Haskell\u4E2D\uFF0C`System.Environment`\u6A21\u584A\
  \u63D0\u4F9B\u8B80\u53D6\u547D\u4EE4\u5217\u53C3\u6578\u7684\u529F\u80FD\u3002\u770B\
  \u4E00\u4E0B\u600E\u9EBC\u7528\uFF1A."
title: "\u8BFB\u53D6\u547D\u4EE4\u884C\u53C2\u6570"
weight: 23
---

## How to: 怎麼做
在Haskell中，`System.Environment`模塊提供讀取命令列參數的功能。看一下怎麼用：

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("你傳入了以下參數: " ++ show args)

-- 假想的終端機輸入:
-- > myprog param1 param2 param3

-- 輸出:
-- 你傳入了以下參數: ["param1", "param2", "param3"]
```

`getArgs`函數傳回一個字串列表，其中包含了所有的命令列參數。

## Deep Dive 深入瞭解
命令列參數的讀取可以讓程序接受用戶或其他程序的輸入。Haskell語言從早期就支持這一功能，這是大多數作業系統終端機的標準做法。

其他的替代方法，例如讀取環境變量或是配置文件，但這些並不適用於臨時參數或一次性任務。對於複雜的命令列參數解析，Haskell社群也發展出了一些更強大的庫，比如`optparse-applicative`，提供了類似GNU的參數設置。

在實作時，命令列參數從操作系統的進程管理中得到，通常在程式啟動時由操作系統提供。在Haskell中，這些參數被作為字串列表呈現，可以透過`getArgs`很容易讀取。

## See Also 參閱資料
- [Haskell System.Environment Documentation](https://hackage.haskell.org/package/base-4.16.1.0/docs/System-Environment.html)
- [optparse-applicative on Hackage](https://hackage.haskell.org/package/optparse-applicative)
- [Real World Haskell, Chapter 8: Command line and file I/O](http://book.realworldhaskell.org/read/io.html)

閱讀官方文檔了解更多庫的用法，而optparse-applicative可以為需要更複雜命令列處理的應用提供更多功能。透過真實世界Haskell書籍學習更多命令列和檔案I/O的實際操作。
