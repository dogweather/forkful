---
title:                "Haskell: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
文字列の長さを求めることに関心がある理由を1-2文で説明する。

あるプログラマーにとって、文字列の長さを知ることは非常に重要なことです。例えば、ユーザーからの入力を処理する際に、文字列の長さを確認し、不正な入力がないかどうかを確認する必要があります。また、プログラムの実行中に文字列の長さを求めることもあり、その結果を利用して処理を行うことができます。

## 方法
文字列の長さを求めるには、幾つかの方法があります。例えば、 `length` 関数を使用することで、文字列の長さを簡単に求めることができます。

```
Haskell
length "こんにちは、世界！"
```

上記のコードを実行すると、`12`という結果が返されます。文字列の日本語文字列を含める場合でも正しく長さを計算してくれることがわかります。

また、文字列の長さを求める別の方法として、文字列を `foldl` 関数を使用してカウントする方法があります。

```
Haskell
foldl (\acc x -> acc + 1) 0 "こんにちは、世界！"
```

このコードも同じ結果である`12`を返します。しかし、`length` 関数よりも少しパフォーマンスが劣る可能性があるので、短い文字列を処理する際にはあまり効率的ではありません。

## ディープダイブ
文字列の長さを求める方法について、さらに深く掘り下げてみましょう。Haskellではバイナリ表現を使用して文字列を表現するので、文字列の長さを求める際にはこのバイナリ表現をどのように扱うかが重要です。

内部的には、文字列は `Data.Text` モジュールで定義される `Text` 型として表現されます。この型はインデックス付きのバイナリ表現として実装されています。そのため、`length`関数や `foldl` 関数を使用する際には、ほとんどの場合`Text`型を直接扱うことができます。

しかし、バイナリ表現として扱うことができる必要がある場合には、 `Data.Text.Encoding` モジュールで提供される関数を使用する必要があります。例えば、UTF-8でエンコードされたバイナリ表現を `Text`型に変換するには、`decodeUtf8`関数を使用します。また、逆に `Text`型をUTF-8でエンコードするには、`encodeUtf8`関数を使用します。

## 参考リンク
- [Haskell Wiki: Strings](https://wiki.haskell.org/Strings)
- [Hackage: Data.Text](https://hackage.haskell.org/package/text)
- [Hackage: Data.Text.Encoding](https://hackage.haskell.org/package/text-encoding-int)