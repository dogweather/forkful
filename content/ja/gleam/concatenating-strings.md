---
title:                "Gleam: 「文字列の連結」"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

Gleamプログラミングのブログへようこそ！

## Why（なぜ）

文字列の連結を行う理由は、異なる文字列を組み合わせて新しい文章を作成するためです。例えば、ある文字列に名前を組み合わせることで、個人に合わせたメッセージを作ることができます。

## How To（方法）

```Gleam
fn concatenate_strings(string1, string2) {
  string1 ++ string2
}

let name = "太郎"
let message = concatenate_strings("こんにちは、", name)

println(message)
```

出力：こんにちは、太郎

## Deep Dive（更に深く）

Gleamでは、2つの文字列を連結するには`++`演算子を使用します。また、複数の文字列を連結することも可能です。また、文字列の連結には`String.concat()`関数を使用することもできます。この関数は、リスト内のすべての文字列を連結して1つの文字列に変換します。

```Gleam
let list_of_strings = ["私の", "名前は", "次郎", "です"]

let message = String.concat(list_of_strings)

println(message)
```

出力：私の名前は次郎です

## See Also（関連リンク）

- [Gleam公式ドキュメント](https://gleam.run)
- [文字列の連結の例](https://github.com/gleam-lang/gleam/blob/master/examples/strings.gleam)
- [文字列操作についての解説](https://www.blacksamurai.org/guide/1-69.html)