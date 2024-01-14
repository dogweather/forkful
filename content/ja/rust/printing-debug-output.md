---
title:    "Rust: デバッグ出力のプリント"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

プログラムを書く際に、デバッグのための出力を表示することは非常に重要です。これにより、プログラムの動作を理解し、問題を特定することができます。Rustでは、デバッグ出力を表示するために「print!」または「println!」マクロを使用できます。

## 方法

以下は、デバッグ出力を表示する方法の例です。まず、プログラムの先頭に「use std::fmt::Debug;」を追加し、デバッグをしたい変数を宣言します。次に、「println!」マクロを使用して変数を出力します。

```Rust
use std::fmt::Debug;

fn main() {
    let num = 10;
    println!("変数「num」の値は{:?}です。", num);
}
```

上記のコードを実行すると、次の出力が得られます。

```
変数「num」の値は10です。
```

デバッグ出力は、プログラムの動作を監視する際にも使用できます。以下は、ループ処理中に値を出力する例です。

```Rust
use std::fmt::Debug;

fn main() {
    for i in 1..=5 {
        println!("ループの回数：{:?}", i);
    }
}
```

上記のコードを実行すると、次の出力が得られます。

```
ループの回数：1
ループの回数：2
ループの回数：3
ループの回数：4
ループの回数：5
```

## 深堀り

デバッグ出力を使用する場合、文字列補間を行いたい場合があります。Rustでは「println!」マクロの中に変数を含めることができますが、より複雑な文字列を扱う場合は、「format!」マクロを使用することをお勧めします。

以下は、「format!」マクロを使用して、デバッグ出力に複数の変数を含める例です。

```Rust
use std::fmt::Debug;

fn main() {
    let num1 = 10;
    let num2 = 20;
    let result = num1 + num2;
    println!("計算結果：{}", format!("{} + {} = {}", num1, num2, result));
}
```

上記のコードを実行すると、次の出力が得られます。

```
計算結果：10 + 20 = 30
```

「format!」マクロを使用することで、より複雑な出力を行うことができます。

## 参考リンク

- [Rust公式ドキュメント - デバッグ出力](https://doc.rust-lang.org/std/fmt/)
- [初めてのRust - プリミティブ型を表示する](https://doc.rust-jp.rs/the-rust-programming-language-ja/version-1.9/book/primitive-types.html#%E3%83%97%E3%83%AA%E3%83%9F%E3%83%86%E3%82%A3%E3%83%96%E5%9E%8B%E3%82%92%E8%A1%A8%E7%A4%BA%E3%81%99%E3%82%8B)
- [Rust by Example - フォーマットマクロ](https://doc.rust-jp.rs/rust-by-example-ja/hello/print/print_format.html)

## 関連リンク

- [よくあるRustのデバッグトリック](https://qiita.com/Tech10/items/614aa16d6a878486e9a4)
- [Rustでのデバッグの方法](https://ryota-ka.me/posts/2017-11-