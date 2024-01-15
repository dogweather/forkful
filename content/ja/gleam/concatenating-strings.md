---
title:                "文字列の連結"
html_title:           "Gleam: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の結合を学ぶことは、プログラミングの重要なスキルの1つです。文字列の結合を使うことで、プログラムをよりスマートかつ効率的に書くことができます。

## 方法

文字列の結合は、以下のように簡単に行うことができます。

```Gleam
let str1 = "こんにちは";
let str2 = "Gleam";
let str3 = str1 ++ " " ++ str2;
```

上記のコードでは、`str1`と`str2`が結合され、新しい変数`str3`に代入されています。最終的な出力は、`こんにちは Gleam`となります。

## ディープダイブ

文字列を結合する方法はいろいろありますが、Gleamでは`++`演算子が最も一般的に使われます。また、文字列の結合にはパフォーマンスの問題があるため、大量の文字列を結合する場合は配列を使うことを検討することが重要です。

## 参考リンク

- [Gleam 公式ドキュメント](https://gleam.run/)
- [Gleam の文字列結合のチュートリアル](https://gleam.run/tour/strings)