---
title:                "文字列の補間"
date:                  2024-01-20T17:51:37.930625-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列補間は、文字列の中に変数や式の値を挿入することです。プログラマーはプログラムの情報をわかりやすく表示したり、動的なメッセージを生成するためにこれを行います。

## How to: (方法)
```Rust
fn main() {
    let name = "世界";
    let greeting = format!("こんにちは、{}！", name);
    println!("{}", greeting);  // 出力: こんにちは、世界！
    
    let hours = 9.5;
    let work_message = format!("あと{}時間で仕事が終わります。", hours);
    println!("{}", work_message);  // 出力: あと9.5時間で仕事が終わります。
}
```

## Deep Dive (深い潜水)
Rustでは、文字列補間には`format!`マクロが使われます。これは`println!`などと同じく内部で動作し、必要に応じて型の変換や書式設定を行います。`format!`は新たな`String`を生成しますが、`println!`はその場で出力します。歴史的には、文字列補間は多くの言語で利用されており、RubyやPythonなどは直接変数を文字列に埋め込む文法をサポートしています。

Rustにおける文字列補間は、プレースホルダーを使い、コンパイル時に型チェックが行われるため、安全性が高いとされています。また、`format!`マクロは`write!`や`writeln!`などに似ていますが、ファイルや標準出力ではなく文字列に対して使います。

```Rust
let error_code = 404;
let error_message = format!("Error: {} - Resource not found.", error_code);
// エラーメッセージは "Error: 404 - Resource not found." と評価されます。
```

他にも、フォーマット指定子でさらなる制御を行うことができます。例えば、小数点以下の桁数を指定したり、埋め込む変数の長さを揃えたりすることが可能です。

## See Also (関連項目)
- Rust Documentation to `std::fmt` module: [https://doc.rust-lang.org/std/fmt/index.html](https://doc.rust-lang.org/std/fmt/index.html)
- The Rust Programming Language book - "Formatting Strings" section: [https://doc.rust-lang.org/book/ch08-02-strings.html#formatting-strings](https://doc.rust-lang.org/book/ch08-02-strings.html#formatting-strings)
- Rust by Example - "Formatted print": [https://doc.rust-lang.org/rust-by-example/hello/print.html](https://doc.rust-lang.org/rust-by-example/hello/print.html)
