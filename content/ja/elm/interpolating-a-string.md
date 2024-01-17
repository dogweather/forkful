---
title:                "文字列を補完する."
html_title:           "Elm: 文字列を補完する."
simple_title:         "文字列を補完する."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## はじめに

こんにちは、エルムプログラマーのみなさん！今回はエルムでよく使われる `インターポレーション`（文字列補間）についてお話しします。`インターポレーション`とは、変数や式を含む文字列をより簡単に作成する方法です。プログラマーはこの機能を使うことで、動的な文字列を生成しやすくなります。

## インターポレーションとは？

例えば、あなたが「今日は<day>です。明日は<tomorrow>です」という文字列を作りたいとしましょう（ここで<day>と<tomorrow>は、実際の値に置き換える必要がある箇所を表します）。これを従来の方法である文字列連結を使って作ると、以下のように書くことになります。

```Elm
let day = "月曜日"
let tomorrow = "火曜日"

let message = "今日は" ++ day ++ "です。明日は" ++ tomorrow ++ "です" 
```

しかし、 `インターポレーション`を使うと、より簡潔に書くことができます。

```Elm
let day = "月曜日"
let tomorrow = "火曜日"

let message = "今日は<day>です。明日は<tomorrow>です" 
```

このように、文字列中に `<変数名>` を書くことで、その変数の値が文字列に自動的に組み込まれます。

## インターポレーションの実際の例

さて、わかりやすくするために実際のコードを見てみましょう。下の例では、 `user` というオブジェクトに `name` と `age` の情報が含まれています。そして、それらの情報を元に、`message`という変数に最終的なメッセージを作ります。

```Elm
let user = 
    { 
        name = "サクラ"
        age = 25
    }

    let message = "私の名前は<user.name>です。年齢は<user.age>歳です。"
```

このように、 `インターポレーション`を使うことで、変数の値を手動で組み込む必要がなくなります。

## # ディープダイブ

`インターポレーション`は他のプログラミング言語でも似たような機能がありますが、エルムでは自動的に文字列をエスケープ（特殊文字を無効化）してくれるので、大変便利です。また、従来の文字列連結よりも正しい形式で変数を組み込めるため、より安全性が高まります。

## 関連情報

- [Elm Documentation - String Interpolation](https://package.elm-lang.org/packages/elm/core/latest/String#format)

- [Elm Japan Community](https://elmjapan.org/)

- [Elm Official Website](https://elm-lang.org/)