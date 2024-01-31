---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

category:             "Rust"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? （何となぜ？）
標準エラー出力（stderr）はエラーメッセージや警告を出すためにある。プログラムの正常な出力を分けて、問題が起きた時にすぐ察知するために使う。

## How to: （方法）
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "エラーが発生しました").unwrap();
}
```

出力:
```
エラーが発生しました
```

## Deep Dive （深堀り）
標準エラー（stderr）はUnix由来で、標準出力(stdout)と分けられている。`println!`はstdoutに書き込むが、`writeln!`に`io::stderr()`を使ってエラーをstderrに出力する。ファイルやネットワークにも同じように書けるが、stderrはコンソールアプリケーションにおいてデバッグやログに便利。

## See Also （関連情報）
- Rustの公式ドキュメント: [std::io](https://doc.rust-lang.org/std/io/)
- Unixにおける標準ストリームの解説: [Standard streams (Wikipedia)](https://en.wikipedia.org/wiki/Standard_streams)
