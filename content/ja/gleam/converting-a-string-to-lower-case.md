---
title:    "Gleam: 文字列を小文字に変換する"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ 

文字列を小文字に変換することに何か意味があるのでしょうか？答えはシンプルです - 文字列を比較するためには、大小文字を無視する必要があるからです。文字列を小文字に変換することで、同じ文字列を異なる大文字で入力されてもプログラムはそれらを同じものとして認識することができます。

## 方法

まず、文字列を小文字に変換するには、`String.to_lower_case()`関数を使用します。例えば、以下のようにコードを記述します。

```
Gleam 1.0.0

import gleam/string

let string = "HELLO WORLD"
let lower_string = string
    |> string.to_lower_case()

```

このコードを実行すると、`lower_string`の値は`"hello world"`になります。文字列を変数に代入せずに、直接以下のように表示することもできます。

```
Gleam 1.0.0

import gleam/string

io.println(string.to_lower_case("HELLO WORLD"))
```

実行すると、コンソールには`"hello world"`と表示されます。

## ディープダイブ

`to_lower_case()`関数は実際には非常にシンプルなものです。内部的には、文字列をリストとして表現し、それぞれの文字を小文字に変換し、最終的にリストを結合して新しい文字列を作成しています。

また、日本語の文字列に対しても同様に機能します。例えば、`"こんにちは"`という文字列を小文字に変換すると、`"こんにちは"`という文字列が返されます。これは、日本語の文字はすでに小文字であるため、変換に差異がないためです。

## 関連リンク

- `String.to_lower_case()`関数のドキュメント: https://gleam.run/modules/gleam_stdlib/gleam-string.html#type:string-to_lower_case/1 
- ASCII規格についての説明: https://ja.wikipedia.org/wiki/ASCII