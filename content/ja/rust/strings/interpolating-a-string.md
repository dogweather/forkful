---
date: 2024-01-20 17:51:37.930625-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u6587\u5B57\u5217\u306E\u4E2D\
  \u306B\u5909\u6570\u3084\u5F0F\u306E\u5024\u3092\u633F\u5165\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u60C5\u5831\u3092\u308F\u304B\u308A\u3084\u3059\u304F\u8868\u793A\u3057\
  \u305F\u308A\u3001\u52D5\u7684\u306A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u751F\u6210\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.799376-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u306F\u3001\u6587\u5B57\u5217\u306E\u4E2D\
  \u306B\u5909\u6570\u3084\u5F0F\u306E\u5024\u3092\u633F\u5165\u3059\u308B\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u60C5\u5831\u3092\u308F\u304B\u308A\u3084\u3059\u304F\u8868\u793A\u3057\
  \u305F\u308A\u3001\u52D5\u7684\u306A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u751F\u6210\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
