---
title:                "テキストファイルの読み込み"
html_title:           "Haskell: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何で？：テキストファイルの読み込みとは何か、そしてプログラマーがそれを行う理由を２〜３文で説明します。

プログラマーがテキストファイルを読み込むのは、コンピューター上で文字情報を処理するためです。例えば、プログラムのソースコードやデータファイルを読み込む際に使用されます。

## 使い方：

```Haskell
main = do
  f <- readFile "example.txt"
  putStrLn f
```

上記のコードは、"example.txt"というファイルを読み込んでその内容を画面に出力するものです。ご覧のように、Haskellではテキストファイルを読み込むためにreadFile関数を使用します。また、プログラム内で読み込んだファイルの内容を変数に格納することもできます。例えば、```f <- readFile "example.txt"```でファイルの内容を変数fに格納しています。

## 詳細：

テキストファイルの読み込みは、古くから使用されていたプログラミングの基本操作の一つです。他にも、バイナリファイルの読み込みやデータベースからのデータの取得といった方法もありますが、テキストファイルの読み込みが最もシンプルで使いやすい方法の一つです。Haskell以外のプログラミング言語でも同様の機能がサポートされています。

## 関連情報：

読み込んだテキストファイルを書き込むこともできます。その場合は、```writeFile```関数を使用します。詳しくはHaskellの公式ドキュメントを参照してください。

参考資料：
- [Haskell 公式ドキュメント](https://www.haskell.org/documentation)