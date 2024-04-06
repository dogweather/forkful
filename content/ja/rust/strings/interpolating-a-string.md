---
date: 2024-01-20 17:51:37.930625-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:37:50.091769-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Rust\u3067\u306F\u3001\u6587\u5B57\u5217\u88DC\u9593\u306B\
  \u306F`format!`\u30DE\u30AF\u30ED\u304C\u4F7F\u308F\u308C\u307E\u3059\u3002\u3053\
  \u308C\u306F`println!`\u306A\u3069\u3068\u540C\u3058\u304F\u5185\u90E8\u3067\u52D5\
  \u4F5C\u3057\u3001\u5FC5\u8981\u306B\u5FDC\u3058\u3066\u578B\u306E\u5909\u63DB\u3084\
  \u66F8\u5F0F\u8A2D\u5B9A\u3092\u884C\u3044\u307E\u3059\u3002`format!`\u306F\u65B0\
  \u305F\u306A`String`\u3092\u751F\u6210\u3057\u307E\u3059\u304C\u3001`println!`\u306F\
  \u305D\u306E\u5834\u3067\u51FA\u529B\u3057\u307E\u3059\u3002\u6B74\u53F2\u7684\u306B\
  \u306F\u3001\u6587\u5B57\u5217\u88DC\u9593\u306F\u591A\u304F\u306E\u8A00\u8A9E\u3067\
  \u5229\u7528\u3055\u308C\u3066\u304A\u308A\u3001Ruby\u3084Python\u306A\u3069\u306F\
  \u76F4\u63A5\u5909\u6570\u3092\u6587\u5B57\u5217\u306B\u57CB\u3081\u8FBC\u3080\u6587\
  \u6CD5\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
