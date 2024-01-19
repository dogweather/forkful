---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

Title: プログラミング言語Elixirで文字列の長さを見つける

## なんだそれは? どうして?
文字列の長さを見つけるとは、文字列が何文字から成るかを計算することです。プログラマーがこれを行う理由は、文字列を操作する際や、バリデーションを行う場面で非常に役立つからです。

## どうやってやるか:
Elixirでは、文字列の長さを計るために`String.length/1`関数を使用します。以下に使用例を示します。

```Elixir
iex> string = "こんにちは、Elixir!"
iex> String.length(string)
10
```

ここでは、Unicode文字列 "こんにちは、Elixir" の長さを計算して、10を返します。

## ディープダイブ
歴史的な文脈：Elixirは2011年に初めて公開され、エンジニアが効率的にソフトウェアを開発できるよう設計されました。その結果、多数の便利な組み込み関数（`String.length/1`もその一つ）が提供されました。

代替方法：`byte_size/1`関数を用いることもできますが、これはバイト単位で長さを返すため、ユニコード文字列に対して正しい結果を返さない可能性があります。

実装の詳細：Elixirの`String.length/1`関数は、与えられたバイナリの各コードポイントを反復処理し、その数をカウントすることで動作します。この方法は、文字列の本当の長さ（つまり、ユーザーが認識する文字数）を返します。

## 参照するべきもの
- 公式Elixirドキュメンテーションで`String.length/1`について詳しく学びましょう: [ここ](https://hexdocs.pm/elixir/String.html#length/1)をクリックしてください。
- Elixirの`byte_size/1`について学びたければ、こちらの[リンク](https://hexdocs.pm/elixir/Kernel.html#byte_size/1)をご覧ください。