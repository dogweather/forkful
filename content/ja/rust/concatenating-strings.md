---
title:    "Rust: 文字列の連結"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## ワイ: ##なぜ 

誰かが文字列を連結することに従事する *#なぜ* について 1-2 文のみで説明します。

文字列を連結する理由はさまざまです。一つは、複数の文字列を一つの大きな文字列にまとめることで、プログラムをより効率的に実行することができることです。また、文字列を連結することでデータを整理し、見やすくすることもできます。それでは、Rust言語で文字列を連結する方法を見ていきましょう！

## 方法: ##方法 

下の ```Rust``` ブロック内にコーディング例とサンプルの出力を記載します。

```Rust
let first_name = "太郎";
let last_name = "山田";
let full_name = format!("{} {}", first_name, last_name);

println!("{}", full_name);

// 出力は "太郎 山田" となります。
```

上の例では、```format!``` マクロを使用して、2つの文字列を連結しています。しかし、Rust言語では他にも文字列を連結する方法があります。例えば、```+``` 演算子を使って2つの文字列を結合することもできます。

```Rust
let first_name = "太郎";
let last_name = "山田";
let full_name = first_name + " " + last_name;

println!("{}", full_name);

// 出力は "太郎 山田" となります。
```

## 深いダイブ: ##深い分析 

文字列を連結する方法は、コードの効率性だけではなく、メモリ管理にも影響を与えます。Rust言語では、文字列を扱う際にメモリの確保や解放を意識する必要があります。そのため、```format!``` マクロを使用して文字列を連結すると、メモリ管理がより効率的に行われます。また、```+``` 演算子を使用する場合は、自分でメモリの確保や解放を行う必要があります。

Rust言語では、メモリ管理に対する厳格な規則があるため、初めは少し難しく感じるかもしれません。しかし、そのおかげでRustプログラムは高い安定性を保ち、クラッシュやメモリリークなどの問題を防ぐことができます。

## さらに見る: ##もっと知る 

- [Rust Stringドキュメント](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust 文字列の初歩](https://gihyo.jp/dev/feature/01/rust/0002)
- [Rust言語を学ぶ: 文字列型](https://edu.clipy-lab.com/learn_rust/003-data_type_strings/)