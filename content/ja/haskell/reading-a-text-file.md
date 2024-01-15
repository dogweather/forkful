---
title:                "「テキストファイルの読み込み」"
html_title:           "Haskell: 「テキストファイルの読み込み」"
simple_title:         "「テキストファイルの読み込み」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読み込むことは、プログラミングで必要不可欠なスキルです。テキストファイルには、ユーザーからの入力やプログラムの出力など、様々な情報が含まれています。Haskellを使ってテキストファイルを読み込む方法を学ぶことで、より多くのタイプのプログラムを作成することができます。

## 使い方

まずは、ファイルを開くための関数をインポートします。

```Haskell
import System.IO
```

次に、openFile関数を使用して、ファイルを読み込みモードで開きます。読み込みモード以外にも、書き込みや追記モードなど、さまざまなモードがあります。

```Haskell
main = do
  handle <- openFile "file.txt" ReadMode
  -- ファイルを読み込む処理を記述
```

ファイルを読み込む処理は、hGetContents関数を使用して、ファイルの内容を文字列として取得します。また、hClose関数を使用して、ファイルを閉じます。

```Haskell
main = do
  handle <- openFile "file.txt" ReadMode
  contents <- hGetContents handle
  putStrLn contents
  hClose handle
```

これで、ファイルの内容がコンソールに表示されます。

## ディープダイブ

openFile関数の第二引数には、どのようなモードを指定することができるのか、詳しく見てみましょう。

| モード      | 説明                     |
|:---------:|:------------------------:|
| ReadMode  | 読み込みモード           |
| WriteMode | 書き込みモード（ファイルが存在しない場合は作成） |
| AppendMode| 追記モード（ファイルが存在しない場合は作成） |
| ReadWriteMode | 読み書きモード |

また、ファイルを開く際に、文字コードを指定することもできます。デフォルトの文字コードは、システムのデフォルト文字コードになります。

上記のコードの例では、ファイルの内容を文字列として取得していますが、hGetChar関数を使用することで、ファイルから1文字ずつ読み込むこともできます。

詳しい内容や、さらに他の関数については、公式ドキュメントを参照してください。

## 参考リンク

- [Haskell 公式ドキュメント](https://www.haskell.org/documentation/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Hoogle - Haskellの関数検索エンジン](https://www.haskell.org/hoogle/)