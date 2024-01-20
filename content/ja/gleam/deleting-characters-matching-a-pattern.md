---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

文字列からパターンに一致する文字を削除するとは、特定のパターンに一致する全ての文字を文字列から取り除くプロセスのことです。プログラマーは、不要なスペースや特殊文字を削除したり、テキストの処理と管理を容易にするために、この技術を頻繁に利用します。

## 方法: (How to:)

以下に、Gleamで文字列からパターンに一致する文字を削除する方法を示します。

```gleam
import gleam/string

fn remove_chars(string: String, pattern: String) -> String {
  string
  |> string.split(pattern)
  |> string.join("")
}

pub fn main(args: List(String)) {
  let test_string = "こんにちは、Gleam!"
  let new_string = remove_chars(test_string, "、")
  new_string |> io.println
}
```

このプログラムを実行すると、出力は以下のようになります。

```
こんにちはGleam!
```

## 詳細な解説 (Deep Dive)

文字列からパターンに一致する文字を削除するとは、大雑把に言うと、文字列の「掃除」です。歴史的に見ると、この技術は初期のコンピューター・プログラミングから存在していました。特に、テキスト解析やデータのクリーニングで頻繁に使用されています。

代替方法としては、文字列関数`replace`を使い、一致するパターンを空の文字列で置き換えるという方法があります。しかし、この方法は些細な違いがあります。具体的には、元の文字列に変更を加えず、新しい文字列を作成します。

Gleamでは、`split`関数と`join`関数を使用してこの処理を行っています。まず、`split`関数で文字列をパターンに一致する部分で区切り、その後`join`関数で部分を結合します。このプロセスで、パターンに一致する部分が新しい文字列から削除されるのです。

## 参照 (See Also)

以下のリンクから関連情報を得ることができます。

- Gleam string module documentation: https://hexdocs.pm/gleam_stdlib/gleam/string.html
- Quick start to Gleam: https://gleam.run/getting-started/
- Gleam lang official guide on string manipulation: https://gleam.run/book/tour/strings.html