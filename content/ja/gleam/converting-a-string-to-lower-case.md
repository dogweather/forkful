---
title:                "文字列を小文字に変換する"
html_title:           "Gleam: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することにかかわる理由は何でしょうか？文字列を処理する際に、大文字と小文字を区別する必要がない場合があるため、文字列を小文字に変換することで処理がより簡単になります。

## 方法

まず、Gleamの標準ライブラリである`String`モジュールを使う必要があります。その後、`to_lower_case`関数を使用して文字列を小文字に変換することができます。

```Gleam
import String

let string = "Hello, World!"
let lower = String.to_lower_case(string)

# Output: "hello, world!"
```

## ディープダイブ

`to_lower_case`関数は、文字列を小文字に変換するための手段として使われますが、実際にはどのように動作しているのでしょうか？この関数では、Unicodeの規則に従って、各文字を小文字に変換します。また、UTF-8エンコーディングをサポートしているため、多言語の文字列を処理する際にも使用することができます。

## See Also

- [Gleam公式ドキュメント：Stringモジュール](https://gleam.run/documentation/std_lib/string/)
- [Unicode正規化と文字の大小比較について](https://blog-ja.textpattern.io/articles/unicode-normalization-and-string-comparison/)
- [UTF-8エンコーディングの仕組みについて](https://ja.wikipedia.org/wiki/UTF-8)