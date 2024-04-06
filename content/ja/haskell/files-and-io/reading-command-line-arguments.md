---
date: 2024-01-20 17:56:08.555527-07:00
description: "How to: (\u65B9\u6CD5) Haskell\u3067\u306F`System.Environment`\u30E2\
  \u30B8\u30E5\u30FC\u30EB\u306E`getArgs`\u95A2\u6570\u3092\u4F7F\u3063\u3066\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\u307E\u3059\u3002\
  \u3053\u306E\u65B9\u6CD5\u306F\u904E\u53BB\u6570\u5341\u5E74\u306B\u6E21\u3063\u3066\
  \u3001\u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\
  \u63A1\u7528\u3055\u308C\u3066\u304D\u307E\u3057\u305F\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.128856-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Haskell\u3067\u306F`System.Environment`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u306E`getArgs`\u95A2\u6570\u3092\u4F7F\u3063\u3066\u30B3\u30DE\u30F3\
  \u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u307F\u307E\u3059\u3002\u3053\u306E\
  \u65B9\u6CD5\u306F\u904E\u53BB\u6570\u5341\u5E74\u306B\u6E21\u3063\u3066\u3001\u591A\
  \u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u63A1\u7528\
  \u3055\u308C\u3066\u304D\u307E\u3057\u305F\u3002"
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
