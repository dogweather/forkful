---
title:                "Haskell: コマンドライン引数の読み取り"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why (なぜ)

コマンドライン引数を読み取ることは、Haskellプログラミングにおける重要なスキルです。コマンドライン引数は、プログラムに実行時に渡す情報を提供するために使用されます。この記事では、コマンドライン引数の読み取り方について学び、その重要性を理解します。

## How To (やり方)

コマンドライン引数を読み取るには、getArgs関数を使用します。これは、コマンドライン引数を文字列のリストとして取得します。以下の例を参考にしてください。

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn "コマンドライン引数:"
    traverse putStrLn args
```

上記の例では、`traverse`関数を使用して、リスト内のすべての要素を表示しています。以下のように実行することで、コマンドライン引数を読み取ることができます。

```bash
runhaskell myprogram.hs arg1 arg2 arg3
```

上記のコマンドを実行すると、以下のような出力が得られます。

```
コマンドライン引数:
arg1
arg2
arg3
```

## Deep Dive (深入り)

コマンドライン引数を扱う際に注意するべきことがあります。まず、リストの最初の要素は、プログラム自体のパスになります。また、特定のデータ型に変換して使用したい場合は、型変換関数を使用する必要があります。例えば、文字列を整数に変換するには、`read`関数を使用します。

また、`putStrLn`関数を使用すると、改行コードが自動的に追加されるため、コマンドライン引数を1行に表示するには、`putStr`関数を使用する必要があります。

## See Also (関連リンク)

- [Haskellのコマンドライン引数の読み取り方](https://www.codementor.io/@julieoconnor/working-with-command-line-arguments-in-haskell-csuuydavh)
- [Haskellドキュメント：Command Line Arguments](https://www.stackbuilders.com/tutorials/haskell/command-line-arguments/)