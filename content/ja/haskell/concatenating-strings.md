---
title:                "文字列の連結"
html_title:           "Haskell: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

あなたが文字列を連結する理由は何でしょうか？Haskellでは、複数の文字列を結合することで、より効率的なデータ操作が可能になり、コードのスタイルが向上するためです。

## 使い方

文字列の連結は、`++`演算子を使用して行います。以下のコードブロックを参考にしてください。

```Haskell
-- 2つの文字列を連結する
"hoge" ++ "fuga"
-- 結果: "hogefuga"
```
```Haskell
-- 変数を使用して文字列を連結する
let x = "こんにちは"
let y = "世界"
x ++ y
-- 結果: "こんにちは世界"
```

`++`演算子を使用することで、任意の数の文字列を連結することができます。

```Haskell
-- 複数の文字列を連結する
"hoge" ++ "fuga" ++ "piyo"
-- 結果: "hogefugapiyo"
```

## 深堀り

Haskellにおける文字列の連結は、文字列をリストとして扱うことで実現されています。つまり、`"hoge"`は文字のリスト`['h', 'o', 'g', 'e']`として扱われ、`++`演算子はリストの結合を行います。そのため、「Hello」という文字列に`"World"`を連結すると、実際にはリストに対して`++`演算子が実行され、最終的には`['H', 'e', 'l', 'l', 'o', 'W', 'o', 'r', 'l', 'd']`という新しいリストが作成されることになります。

## 参考リンク

- [Hackage: String](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html)
- [Learn you a Haskell for Great Good! - Concatenation](http://learnyouahaskell.com/starting-out#string)
- [プログラミングHaskell](https://www.amazon.co.jp/%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%9F%E3%83%B3%E3%82%B0Haskell-Graham-Hutton/dp/427406856X/)