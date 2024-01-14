---
title:                "Rust: パターンに一致する文字の削除"
simple_title:         "パターンに一致する文字の削除"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

Rustを使って文字をパターンにマッチさせて削除することが必要な理由について、1-2文で説明します。

## 方法

パターンにマッチする文字を削除する方法のコーディング例とサンプル出力を「```Rust ... ```」のコードブロックで紹介します。

```Rust
// 文字列を定義
let mut text = String::from("Hello, World!");

// パターンを定義
let pattern = "lo";

// 文字列からパターンを探し、マッチした場合は削除する
while text.contains(pattern) {
    text = text.replace(pattern, "");
}

// 結果を出力
println!("{}", text); // 出力: He, World!
```

## 深堀り

パターンマッチングについて、もっと詳しく説明します。Rustでは`replace()`メソッドを使って、文字列から指定したパターンを検索して置換することができます。このメソッドは、最初にマッチしたパターンのみを置換するため、すべてのマッチした箇所を削除するためにはループを使う必要があります。さらに、マッチしない場合は元の文字列をそのまま返すため、`contains()`メソッドを使ってループを制御する必要があります。このように、Rustではシンプルなコードでも様々な方法でパターンマッチングを利用することができます。

## その他のリンク

- Rustプログラミング言語: https://www.rust-lang.org/ja/
- Rust公式ドキュメント: https://doc.rust-lang.org/