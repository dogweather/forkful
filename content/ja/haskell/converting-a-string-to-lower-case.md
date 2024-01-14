---
title:                "Haskell: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することに興味が湧いていますか？それは、プログラミングにおいて文字列を操作する必要がある一般的なタスクの一つであり、小文字に変換することで文字列の処理がより簡単になるからです。

## 方法
Haskellでは、`map`と`toLower`という2つの関数を使用して、文字列を小文字に変換することができます。例を見てみましょう。

```Haskell
-- "Hello World!"を小文字に変換する例
let str = "Hello World!"
let lowerStr = map toLower str

-- 出力: "hello world!"
```

`map`関数は、リストや文字列の各要素に対して指定した関数を適用し、新しいリストや文字列を返す関数です。`toLower`関数は、与えられた文字を小文字に変換する関数です。これら2つの関数を組み合わせることで、簡単に文字列を小文字に変換することができます。

## 深堀り
上記の方法では、文字列を全て小文字に変換することができましたが、実際にはASCII文字だけでなく、Unicode文字も含めて変換する必要があるかもしれません。その場合は標準ライブラリの`Data.Text`モジュールの`toLower`関数を使用することで、全ての文字を適切に小文字に変換することができます。また、文字列の大文字を小文字に変換するだけでなく、逆の変換を行う`toUpper`関数も存在します。

## はじめに戻る
文字列を小文字に変換する方法について紹介しましたが、他にも文字列を操作するための多くの関数が存在します。Haskellの標準ライブラリを調べてみることで、より多くの機能を見つけることができるでしょう。

## 言語参考
- [Haskell Wiki: 関数型プログラミング](https://wiki.haskell.org/Functional_programming)
- [Haskell Wiki: `map`関数](https://wiki.haskell.org/Map)
- [Haskell Wiki: `toLower`関数](https://wiki.haskell.org/Lowercase)
- [Haskell Wiki: Unicodeサポート](https://wiki.haskell.org/Unicode)
- [Haskellの標準ライブラリのドキュメント](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#g:6)