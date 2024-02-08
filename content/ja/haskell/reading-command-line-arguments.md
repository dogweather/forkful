---
title:                "コマンドライン引数の読み取り"
aliases:
- ja/haskell/reading-command-line-arguments.md
date:                  2024-01-20T17:56:08.555527-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
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
