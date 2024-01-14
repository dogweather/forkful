---
title:                "Haskell: 文字列の長さを見つける"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ？

文字列の長さを求めることによって、プログラムの実行中に必要なデータのサイズを把握することができます。これはプログラムの効率性を向上させるのに役立ちます。

## 手順

文字列の長さを求めるには、Haskellの `length` 関数を使用します。以下の例を参考にしてください。

```Haskell
-- 文字列を定義
let str = "こんにちは"

-- 文字列の長さを求める
length str
```

上記のコードを実行すると、出力として `5` が得られます。これは "こんにちは" という文字列が5つの文字で構成されていることを意味します。

## 深堀り

Haskellの `length` 関数は、リストの要素の数を数えるために使用されます。文字列もリストとして扱われるため、 `length` 関数を使用することで文字列の長さを求めることができます。ただし、文字列の長さを求める際は、アルファベット1文字が1つの要素として数えられるため、全角文字を扱う場合は注意が必要です。

## 関連記事

[Learn You a Haskell for Great Good! - Strings](http://learnyouahaskell.com/starting-out#strings)  
[Haskellで文字列の扱い方を学ぶ](https://qiita.com/lotz/items/32a9596b64acfe059397)