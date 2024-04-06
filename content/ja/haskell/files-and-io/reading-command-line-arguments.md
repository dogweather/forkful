---
date: 2024-01-20 17:56:08.555527-07:00
description: "How to: (\u65B9\u6CD5) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B\uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.069132-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

## How to: (方法)
```haskell
import System.Environment (getArgs)

-- main関数ではコマンドライン引数を取得してprintします。
main :: IO ()
main = do
  args <- getArgs
  print args
```

サンプル出力：

```
$ runhaskell args.hs arg1 arg2 arg3
["arg1","arg2","arg3"]
```

## Deep Dive (掘り下げ)
Haskellでは`System.Environment`モジュールの`getArgs`関数を使ってコマンドライン引数を読みます。この方法は過去数十年に渡って、多くのプログラミング言語で採用されてきました。

他にもコマンドライン引数を解析するためのライブラリ（例：`optparse-applicative`）があり、より複雑な引数のパターンを簡単に扱えます。

実装の詳細として、`getArgs`はIOモノイドの中で使われ、プログラムの実行時に一度だけ値を取得することができます。これはHaskellの純粋性を保ちつつ、外部の世界と対話する方法です。

## See Also (関連情報)
- The Haskell 98 Report: System.Environment: https://www.haskell.org/onlinereport/standard-prelude.html#sect36.3
- `optparse-applicative` ライブラリのHackageページ: https://hackage.haskell.org/package/optparse-applicative
- Real World Haskellのコマンドライン引数の扱いに関する章: http://book.realworldhaskell.org/read/io.html
