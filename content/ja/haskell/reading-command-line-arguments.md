---
title:                "Haskell: コンピューター・プログラミングにおける「コマンドライン引数の読み込み」"
simple_title:         "コンピューター・プログラミングにおける「コマンドライン引数の読み込み」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
プログラミングをする際に、コマンドライン引数を読み取ることは非常に重要です。引数を正しく読み取ることで、プログラムの挙動をカスタマイズしたり、ユーザーとやり取りすることができます。

## 方法
Haskellでコマンドライン引数を読み取る方法を見てみましょう。まず、"System.Environment"モジュールをインポートします。

```Haskell
import System.Environment
```

次に、"getArgs"関数を使って引数を取得します。この関数は、"IO [String]"という型を持っています。それでは、実際にコマンドライン引数を読み取る関数を作ってみましょう。

```Haskell
getCommandLineArgs :: IO [String]
getCommandLineArgs = getArgs
```

このように定義した後、main関数内で"getCommandLineArgs"を呼び出し、引数を取得することができます。

```Haskell
main = do
    args <- getCommandLineArgs
    putStrLn ("The arguments are: " ++ show args)
```

では、"Hello World!"という文字列を引数としてプログラムを実行してみましょう。ターミナル上で以下のように入力します。

```Shell
runhaskell myProgram.hs "Hello World!"
```

すると、プログラムは以下のような出力をします。

```Shell
The arguments are: ["Hello World!"]
```

無事に引数を読み取ることができましたね！

## 深堀り
コマンドライン引数をもっと詳しく見ていきましょう。上で紹介した"getArgs"関数は、実際には"IO [String]"という型を持っています。この型は、"IO"モナドを使用する必要があることを意味しています。Haskellでは、"IO"モナドを使用することで、実行時に副作用を持つ処理を行うことができます。

また、"getArgs"関数は実行時の引数をすべてリストとして返すため、引数を一つずつ取り出して処理する際には、リスト操作を行う必要があります。例えば、以下のように引数を１つずつ取り出し、それぞれの引数に対して処理を行う関数を作ることができます。

```Haskell
processArg :: String -> IO ()
processArg arg = putStrLn ("The argument is: " ++ arg)

main = do
    args <- getCommandLineArgs
    mapM_ processArg args
```

実行時の引数が複数ある場合でも、それぞれの引数に対して処理が行われます。

## 参考リンク
- [Haskellでのコマンドライン引数の読み取り](https://medium.com/@jonathandekhtiar/getting-command-line-options-in-haskell-d75be741c960)
- [System.Environmentモジュールのドキュメント](https://hackage.haskell.org/package/base-4.12.0.0/docs/System-Environment.html)
- [Control.Monadモジュールのドキュメント](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html)