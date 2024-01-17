---
title:                "文字列の大文字化"
html_title:           "Rust: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

#Rustにおける大文字化（Capitalization）

## What & Why?

大文字化とは、文字列をすべて大文字に変換することを指します。プログラマーがこれを行う理由は、文字列を比較するために使われるケースインセンシティブ性（大文字と小文字を区別しない性質）を実現するためです。

## How to:
```Rust
let string = "hello world";
let capitalized_string = string.to_uppercase();
println!("Capitalized string is: {}", capitalized_string);
```

出力結果：
```
Capitalized string is: HELLO WORLD
```

## Deep Dive:

### ヒストリカルコンテキスト

文字列の大文字化は、元々は印刷物で使用される英語の大文字と小文字の変換方法に影響を受けています。印刷機の主な文字は大文字であり、小文字は手書きの文字として扱われていました。しかし、コンピューターの普及により、大文字と小文字の区別が重要になりました。その結果、プログラムやウェブサイトなどでは大文字と小文字を区別せずに文字列を扱うことが一般的になりました。

### 代替手段

Rustでは、文字列の大文字化には`to_uppercase()`メソッドが使用できますが、他にも様々な方法があります。例えば、`to_ascii_uppercase()`メソッドを使うとASCII文字のみを大文字化することができます。また、正規表現を使って自分で大文字化する方法もあります。

### 実装の詳細

Rustでは、`to_uppercase()`メソッドは`std::ascii::AsciiExt`トレイトに定義されています。このトレイトはASCII文字列専用のメソッドを提供しており、非ASCII文字の大文字化には`std::unicode::UnicodeExt`トレイトを使用します。

## See Also:
- [Rust documentation on string methods](https://doc.rust-lang.org/std/string/struct.String.html#method.to_uppercase)
- [Comparison of string methods in different languages](https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(string_functions)#Case_conversions)