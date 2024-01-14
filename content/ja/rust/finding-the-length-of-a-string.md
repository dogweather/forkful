---
title:    "Rust: 文字列の長さを見つける"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# なぜ

Rustは強力なプログラミング言語で、高速で安全なソフトウェアを開発することができます。文字列の長さを求めることは、プログラミングにおいて非常に一般的なタスクであり、多くの場面で必要になります。Rustを使って文字列の長さを求める方法を学ぶことで、より効率的なコードを書くことができるようになります。

# 方法

文字列の長さを求めるには、Rustの標準ライブラリにある`len()`メソッドを使います。下のコードでは、`len()`メソッドを使用して文字列の長さを求めています。

```Rust
let s = "Hello Rust!";
let length = s.len();

println!("The length of the string is: {}", length);

// 出力:
// The length of the string is: 11
```

上のコードでは、まず変数`s`に文字列を格納し、`len()`メソッドで文字列の長さを取得しています。そして、`println!()`マクロを使って、文字列の長さを出力しています。このように、Rustでは非常に簡単に文字列の長さを求めることができます。

# ディープダイブ

Rustの`len()`メソッドは、文字列のバイト数を返すため、日本語などのマルチバイト文字を含む文字列の場合、意図した結果が得られない可能性があります。そのため、マルチバイト文字を含む文字列の長さを求める場合は、`chars()`メソッドを使って、文字数を数える必要があります。下のコードでは、これらのメソッドを組み合わせて、文字数を数えています。

```Rust
let s = "こんにちは Rust!";

// バイト数を取得
let bytes = s.len();
println!("バイト数: {}", bytes);

// 文字数を取得
let chars = s.chars().count();
println!("文字数: {}", chars);

// 出力:
// バイト数: 19
// 文字数: 10
```

文字数を数えるには、`chars()`メソッドを使って文字のイテレーターを取得し、`count()`メソッドで文字数を数えます。このように、Rustでは異なる言語にも対応した文字列の長さを求めることができます。

# 関連リンク

- [Rustの標準ライブラリのString型ドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Rustの標準ライブラリのstr型ドキュメント](https://doc.rust-lang.org/std/primitive.str.html#method.len)
- [Rust by ExampleのStringsチュートリアル](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust for BeginnersのStringsチュートリアル](https://github.com/LearningRust/strings)