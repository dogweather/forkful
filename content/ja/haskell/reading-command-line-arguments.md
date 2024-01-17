---
title:                "コンピュータプログラミングの記事「コマンドライン引数を読む」"
html_title:           "Haskell: コンピュータプログラミングの記事「コマンドライン引数を読む」"
simple_title:         "コンピュータプログラミングの記事「コマンドライン引数を読む」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

["

## 何とどうして？
コマンドライン引数の読み取りとは、起動時にプログラムに渡されるデータを取得することです。プログラマーは、ユーザーからの入力や環境に応じてプログラムの動作を変えるためにこの機能を使用します。

## 実際にやってみよう
以下のコードを使用して、Haskellでコマンドライン引数を読み取る方法をご紹介します。

```Haskell
import System.Environment (getArgs)
main = do
    args <- getArgs
    putStrLn ("引数: " ++ show args)
```

コマンドラインでプログラムを実行する際に、引数として任意の値を与えることができます。例えば、以下のように実行します。

```
$ runhaskell example.hs foo bar baz
```

実行結果は、
```
引数: ["foo", "bar", "baz"]
```
となります。

## 詳しく掘り下げる
コマンドライン引数の読み取り機能は、Haskellだけでなく他の言語でもよく使用されています。代表的なものとして、Pythonの`sys.argv`やC++の`argc`と`argv`があります。

Haskellでは、`System.Environment`モジュールの`getArgs`関数を使用してコマンドライン引数を取得します。この関数は、プログラム起動時に渡された全ての引数を文字列のリストとして返します。

## 参考リンク
- [HaskellのgetArgs関数ドキュメント](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getArgs)
- [Pythonのsys.argvドキュメント](https://docs.python.org/3/library/sys.html#sys.argv)
- [C++のargcとargvドキュメント](https://en.cppreference.com/w/cpp/language/main_function)