---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラーとは、プログラムがエラーメッセージを出力するための特別なデータストリームのことです。プログラミングでは、正しい出力とエラーメッセージを分けるために使います。

## How to:
Gleamで標準エラーに書き込む:

```gleam
import gleam/io

pub fn main() {
  io.print("普通の出力\n")
  io.eprint("エラー発生!\n")
}
```

出力例:

```
普通の出力
エラー発生!
```

## Deep Dive
標準エラーはUNIXから始まります。`io:eprint`関数はstderrに直接書き込む。Gleam以外にも、多くの言語で似た機能があります。Gleamでは、`Result`型でエラーを扱い、適切な時に標準エラーに出力するのが一般的です。

## See Also
- Rustのエラーハンドリング: [https://doc.rust-lang.org/book/ch09-00-error-handling.html](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- UNIXの標準ストリームの歴史: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)