---
title:                "Elm: 文字列の連結"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列を連結する理由は、プログラミングで特定の情報を1つの文字列としてまとめる必要があるからです。例えば、名前と苗字を連結してフルネームを作成する場合や、日付を表す文字列を生成する場合などがあります。

## 方法

```Elm
name = "山田"
surname = "太郎"
fullName = name ++ " " ++ surname
```

上記のコードでは、2つの文字列を連結してフルネームを作成しています。`++`演算子を使用することで、簡単に文字列を結合することができます。また、日付を表す文字列を生成する場合は、`String.fromInt`関数を使用して数値を文字列に変換することもできます。

```Elm
year = 2020
month = 6
day = 24
dateString = String.fromInt year ++ "/" ++ String.fromInt month ++ "/" ++ String.fromInt day
```

上記のコードでは、数値を文字列に変換して日付を表す文字列を作成しています。

## 深堀り

文字列の連結には、純粋な方法と効率的な方法の2つがあります。純粋な方法は、`++`演算子を使用して1つの文字列に結合する方法です。この方法は、コードがシンプルで読みやすくなりますが、大きな文字列を結合する場合にはパフォーマンスの問題が生じる可能性があります。

一方、効率的な方法は、`List`モジュールの`intercalate`関数を使用する方法です。この関数は、リスト内の複数の要素を指定した区切り文字で連結することができます。例えば、`intercalate "," ["a", "b", "c"]`は、文字列`"a,b,c"`を作成します。この方法は、大きな文字列を結合する場合にも高速であり、パフォーマンスを気にする場合にはおすすめです。

## 参考サイト

- [Elm公式サイト](https://elm-lang.org/)
- [Learn X in Y minutes - Elm](https://learnxinyminutes.com/docs/elm/)
- [Elm Documentation](https://package.elm-lang.org/packages/elm/core/latest/)
- [Elm in Japanese](https://elmjapan.org/)