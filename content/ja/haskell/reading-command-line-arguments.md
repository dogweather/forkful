---
title:                "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
html_title:           "Haskell: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why
プログラマーであれば、プログラムを実行するにあたってコマンドライン引数を読み取ることが重要です。コマンドライン引数を読み取ることで、プログラムに対して柔軟性を持たせることができます。

## How To
コマンドライン引数を読み取るには、`System.Environment`モジュールの`getArgs`関数を使用します。例えば、以下のようなプログラムを作成し、実行するとコマンドライン引数のリストが出力されます。

```Haskell
import System.Environment

main = do
    args <- getArgs
    print args
```

入力：`runhaskell program.hs hello world`

出力：`["hello", "world"]`

## Deep Dive
コマンドライン引数を取得するには、2つの方法があります。まず、コマンドライン引数の数を取得するための`getArgs`関数を使用し、`length`関数を用いて数を取得することができます。また、リストの要素を直接アクセスすることもできます。例えば、コマンドライン引数のリストの先頭の引数を取得するには、`!!0`と書くことができます。

## See Also
- [HaskellのSystem.Environmentモジュールのドキュメント](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html)
- [HaskellコードライブラリーのgetArgs関数の例](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html#v:getArgs)