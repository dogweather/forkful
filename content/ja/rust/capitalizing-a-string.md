---
title:    "Rust: 文字列の大文字化"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ
文字列の大文字変換を行う理由を説明します。

## やり方
Rustコードの例と` ```Rust ... ``` `のコードブロック内のサンプル出力を使用して、文字列の大文字変換の方法を説明します。

```Rust
let my_string = "こんにちは！";
let capitalized_string = my_string.to_uppercase();
println!("{}", capitalized_string);
```
出力:
```
こんにちは！
```

## 深堀り
文字列の大文字変換についてのより詳しい情報を紹介します。Rustの標準ライブラリには、`to_uppercase()`メソッドを使用することで文字列を簡単に大文字に変換することができます。また、UTF-8文字列の大文字変換には注意が必要です。

## See Also
- [Rust標準ライブラリドキュメント](https://doc.rust-lang.org/std/string/struct.String.html)
- [UTF-8の大文字変換についての詳細情報](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)

この記事では、Rustで文字列の大文字変換を行う方法を紹介しました。`to_uppercase()`メソッドを使用することで、簡単に文字列を大文字に変換することができます。しかし、UTF-8文字列の場合には注意が必要です。詳細情報はドキュメントを参照してください。