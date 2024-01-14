---
title:                "Rust: 文字列の連結"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結を行う理由は、Rustでは複数の文字列を組み合わせて新しい文字列を作成する必要があるためです。これにより、より複雑な文字列を効率的に扱うことができます。

## 方法

Rustでは、`+`演算子を使用して文字列を結合することができます。例えば、以下のように書くことができます。

```Rust
let str1 = "Hello, ";
let str2 = "world!";
let combined_str = str1 + str2;
println!(combined_str);
```

出力は次のようになります。

```
Hello, world!
```

また、複数の文字列を結合する場合は、`format!`マクロを使用することもできます。例えば、以下のように書くことができます。

```Rust
let str1 = "Hello";
let str2 = ", ";
let str3 = "world!";
let combined_str = format!("{}{}{}", str1, str2, str3);
println!(combined_str);
```

出力は次のようになります。

```
Hello, world!
```

## 深堀り

文字列の結合に関する詳細な情報は、Rustのドキュメントを参照することができます。また、`str`型や`String`型のメソッドを使用することで、文字列をより細かく扱うこともできます。

## 参考リンク

- [The Rust Programming Language](https://www.rust-lang.org/ja)
- [Rustドキュメント](https://doc.rust-lang.org/)
- [str型](https://doc.rust-lang.org/std/primitive.str.html)
- [String型](https://doc.rust-lang.org/std/string/struct.String.html)