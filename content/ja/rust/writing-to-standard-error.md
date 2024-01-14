---
title:    "Rust: 標準エラーに書き込む"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ考えるのか

Rustはセルフホスティング型言語であり、高速で安全なシステムプログラミングを可能にします。そのため、プログラマーにとってRustは魅力的な選択肢となります。

また、Rustはエラーハンドリングに強い言語であり、エラーを適切に処理することが重要です。そのため、標準エラーへの書き込みをすることは重要なスキルとなります。

## 方法

標準エラーへの書き込みは、標準ライブラリである`std::io::stderr()`メソッドを使用します。以下のようにコードを記述することで、`Hello World!`を標準エラーに表示することができます。

```Rust
use std::io::{stderr, Write};

fn main() {
    let mut stderr = stderr();  // 標準エラーへのハンドルを得る
    stderr
        .write(b"Hello World!\n") // bを使用して、バイト列に変換する
        .unwrap();               // エラーハンドリングは必ず行う
}
```

実行すると、標準エラーに`Hello World!`が表示されます。

## 深堀り

標準エラーへの書き込みは、エラーハンドリングを行う際に非常に重要な役割を果たします。例えば、プログラムがエラーを起こした際に、標準エラーを使用してエラーメッセージを表示することができます。

また、標準エラーは標準出力とは異なり、テキストベースではなくバイトベースで処理されます。そのため、テキストで出力する際にはバイト列に変換する必要があります。バイト列の先頭には必ず`b`を付けることを忘れないようにしましょう。

## See Also

- [The Rust Programming Language](https://www.rust-lang.org/)
- [Writing to Standard Error in Rust](https://www.perficient.com/insights/trends/writing-to-standard-error-in-rust)
- [The Rust Book](https://doc.rust-lang.org/book/)

Rustの標準エラーへの書き込みは、エラーハンドリングに不可欠な技術です。ぜひこの記事を参考に、より安全で高速なRustプログラミングを楽しんでください。