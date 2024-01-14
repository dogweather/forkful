---
title:                "Haskell: 文字列のキャピタル化"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングを行う人々は、しばしば文字列を大文字に変換する必要があります。例えば、ユーザーからの入力を正しく処理するためには、大文字と小文字を区別する必要があります。また、文字列を比較する場合にも、大文字と小文字を区別する必要があります。このような場面で、文字列を大文字に変換することは非常に重要なテクニックです。

## 方法

文字列を大文字に変換する方法はさまざまですが、今回はHaskellを使って実装してみましょう。

まず、文字列を受け取る関数capitalizeを定義します。

```Haskell
capitalize :: String -> String
capitalize str = undefined
```

この関数では、文字列を引数として受け取り、大文字に変換した文字列を返すように定義します。Haskellではパターンマッチングを使って、文字列を一つずつ分解して処理することができます。

```Haskell
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : capitalize xs
```

このように、文字列を再帰的に分解しながら大文字に変換していきます。toUpper関数は、与えられた文字を大文字に変換する関数です。

実際に、"haskell"という文字列を大文字に変換してみましょう。

```Haskell
capitalize "haskell" 
```
```
HASKELL
```

## ディープダイブ

この方法では、文字列に含まれる全ての文字を大文字に変換します。もし特定の文字だけ大文字にしたい場合は、パターンマッチングを使って条件分岐を行うことで実現することができます。

また、現在はASCII文字しか扱っていませんが、Unicode文字にも対応することができます。Unicode文字を扱うには、Data.Textモジュール内のtoUpper関数を使用する必要があります。

さらに、今回は文字列を再帰的に分解して処理しましたが、Haskellにはより効率的な方法があります。例えば、Data.Textモジュール内のtoUpper関数を使うことで、特定の文字だけ大文字に変換することもできます。

```Haskell
Data.Text.map toUpper "haskell"
```
```
"HASKELL"
```

## 参考リンク

- [Haskellで文字列を大文字に変換する方法](https://www.yesodweb.com/book/shakespearean-templates.html#shakespearean-環境)
- [Haskell標準ライブラリのData.Textモジュール](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Haskellにおけるパターンマッチングの基礎](https://medium.com/彼女と彼氏の研究日誌/haskell-におけるパターンマッチングの基礎-79972a9b5275)

## 関連リンク

- [Haskell入門ブログ](https://haskelltutorial.net/)
- [Haskell.jp公式ウェブサイト](https://haskell.jp/)
- [Haskell日本ユーザーグループ](https://haskell.jp/users.html)