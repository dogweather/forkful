---
title:                "Haskell: テキストの検索と置換"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストを検索して置換する作業を行う理由は様々です。例えば、大量のファイルの中から特定の単語を見つけて置き換える必要がある場合や、古いコードを新しいものに更新する必要がある場合などが挙げられます。

## 使い方

まず、Haskellのパッケージ管理ツールであるCabalを使用して、Textパッケージをインストールします。

```Haskell
cabal install text
```

次に、置換を行いたいファイルを読み込みます。例として、ファイル名を「input.txt」とします。

```Haskell
import Data.Text as T

main = do
    input <- readFile "input.txt"
```

ファイルを読み込んだ後、置換を行います。以下の例では、文字列「hello」を「こんにちは」に置換しています。

```Haskell
let output = replace "hello" "こんにちは" input
```

最後に、新しいファイルを作成して、置換を行った内容を書き込みます。

```Haskell
writeFile "output.txt" output
```

以上のコードを実行すると、ファイル「output.txt」には置換されたテキストが書き込まれます。

## 深堀り

Textパッケージを使用して、置換の他にも検索や編集などのテキスト処理を行うことができます。例えば、次のようにして文字列を検索することができます。

```Haskell
let search = T.isInfixOf "apple" input

-- inputに「apple」が含まれていれば、searchはTrueを返す
```

また、テキストを分割してリストに格納することもできます。

```Haskell
let list = T.splitOn " " input

-- 空白で区切られた単語をリストに格納する
```

さらに、正規表現を使用してテキストを操作することもできます。

```Haskell
let output = T.replaceRegex "\\d+" "number" input

-- 数字を"number"に置換する
```

Textパッケージの詳細な使い方や、他の便利な関数については公式ドキュメントを参照してください。

## 参考リンク

- [Haskell 公式ドキュメント](https://www.haskell.org/documentation/)
- [Haskell Textパッケージの使用方法](https://hackage.haskell.org/package/text-1.2.3.0/docs/Data-Text.html)
- [正規表現についてのチュートリアル](https://www.rexegg.com/regex-quickstart.html)

## 関連記事

- [Haskellでファイルの読み書きを行う方法](https://example.com/article/haskell-file-io)
- [正規表現を使用したテキスト処理の方法](https://example.com/article/regex-text-processing)