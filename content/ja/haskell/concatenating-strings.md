---
title:                "Haskell: 文字列の連結"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

こんにちは、Haskellプログラミングブログの読者の皆さん。今日の記事では、私たちが一緒に文字列を連結する方法についてお話しします。なぜ文字列を連結する必要があるのか、どのようにしてコードを記述するか、さらに深く掘り下げてみましょう。

## Why

文字列を連結することは、複数のテキストデータを一つの文字列にまとめるために必要です。例えば、ユーザーに入力してもらった名前と苗字を組み合わせて、挨拶のメッセージを作成する際に使用することができます。また、データベースから取得した情報を文字列として表示する際にも必要です。

## How To

文字列を連結するためには、Haskellの```++```演算子を使用します。例えば、以下のようにコードを記述することで文字列を連結することができます。

```Haskell
main :: IO ()
main = do
  let name = "太郎"
  let last_name = "山田"
  putStrLn ("こんにちは、" ++ name ++ last_name ++ "さん！")
```

上記のコードでは、先ほどの例で述べたように名前と苗字を連結して挨拶のメッセージを作成し、```putStrLn```関数を使用して表示しています。実行結果は以下のようになります。

```
こんにちは、太郎山田さん！
```

## Deep Dive

Haskellでは、文字列は文字型のリストとして扱われます。つまり、```String```は```[Char]```と同じ意味になります。そのため、文字列を連結するには、実際にはリスト同士を結合していることになります。

Haskellでは、文字列をリストとして扱うために便利な関数や演算子が用意されています。例えば、```show```関数を使用することで、数値や文字を文字列に変換することができます。また、```++```演算子の代わりに```concat```関数を使用することで、複数のリストを一つに結合することもできます。

さらに、Haskellでは文字列を連結する方法の他にも、文字列の一部を取得したり、置換したりする方法も提供されています。これらの機能を組み合わせることで、より複雑な文字列操作が可能になります。

See Also

- [Haskell 公式チュートリアル](https://www.haskell.org/tutorial/)
- [HaskellWiki の文字列操作のページ](https://wiki.haskell.org/String_operations)
- [Real World Haskell の文字列操作のセクション](http://book.realworldhaskell.org/read/strings.html)