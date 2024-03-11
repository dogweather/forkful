---
date: 2024-01-20 17:56:08.555527-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u8D77\u52D5\u3059\u308B\u6642\
  \u306B\u5916\u90E8\u304B\u3089\u4E0E\u3048\u3089\u308C\u308B\u30D1\u30E9\u30E1\u30FC\
  \u30BF\u3092\u53D7\u3051\u53D6\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u305D\u308C\u3092\u4F7F\u3063\u3066\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u632F\u308B\u821E\u3044\u3092\u52D5\u7684\u306B\u5909\u3048\u305F\u308A\
  \u3001\u30E6\u30FC\u30B6\u306E\u5165\u529B\u3092\u51E6\u7406\u3057\u305F\u308A\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.777852-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u8D77\u52D5\u3059\u308B\u6642\
  \u306B\u5916\u90E8\u304B\u3089\u4E0E\u3048\u3089\u308C\u308B\u30D1\u30E9\u30E1\u30FC\
  \u30BF\u3092\u53D7\u3051\u53D6\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u305D\u308C\u3092\u4F7F\u3063\u3066\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u632F\u308B\u821E\u3044\u3092\u52D5\u7684\u306B\u5909\u3048\u305F\u308A\
  \u3001\u30E6\u30FC\u30B6\u306E\u5165\u529B\u3092\u51E6\u7406\u3057\u305F\u308A\u3057\
  \u307E\u3059\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数を読むとは、プログラムが起動する時に外部から与えられるパラメータを受け取ることです。プログラマはそれを使って、プログラムの振る舞いを動的に変えたり、ユーザの入力を処理したりします。

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
