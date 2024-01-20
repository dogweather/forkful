---
title:                "文字列の長さを見つける"
html_title:           "Elm: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何となく？

文字列の長さを調べるとは、その文字列が何文字から構成されているかを確認することです。大抵のプログラマーは、配列や文字列の長さを操作するためや、入力の有効性を確認するためにこれを用います。

## やってみよう：

文字列の長さを調べる方法を学ぶために、以下のGleamのコード例題を見てみましょう：

```Gleam
import gleam/string

fn main() {
  let text = "こんにちは、世界"
  let length = string.length(text)
  io.println(length)
}
```

上記のコードを実行すると、文字列`"こんにちは、世界"`の長さ、つまり文字数が出力されます。

## 深堀り：

1. **歴史的文脈**：古くからの多くのプログラミング言語では、文字列の長さを調べるための組み込み関数が用意されています。その中には、Gleamや、その他の関数型PLも含まれます。

2. **代替案**：多くの言語（Gleamを含む）では、組み込み関数を使う以外にも、ループを使って一文字ずつ数える方法など、独自に文字列の長さを計算する方法を用いることも可能です。

3. **実装の詳細**: Gleamの`string.length`関数は内部的にはErlangの`byte_size`関数を使用しています。この方法では、UTF-8エンコードされた文字列のバイト数が返されるため、含まれている全ての文字を正確にカウントします。

## 関連情報：

関連するリソースについては、以下のリンクをチェックしてみてください：

- Gleamの公式ドキュメンション：[文字列](https://gleam.run/book/tour/strings.html)