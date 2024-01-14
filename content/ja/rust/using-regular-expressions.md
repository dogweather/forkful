---
title:    "Rust: 正規表現の使用"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由はさまざまですが、主な目的は文字列のパターンマッチングや置換を行うことです。たとえば、メールアドレスや電話番号などの特定の書式に一致する文字列を抽出したり、文字列内の特定の単語を検索したりすることができます。

## 使い方

Rustでは、[`regex`](https://docs.rs/regex/1.5.4/regex/)というクレートを使用して正規表現を扱うことができます。まず、Cargo.tomlファイルに`regex`を依存関係として追加します。

```
[dependencies]
regex = "1.5.4"
```

次に、コード内で`regex`クレートをインポートします。

```
use regex::Regex;
```

これで準備が整いました。以下は、正規表現を使用して文字列から特定のパターンに一致する部分を抽出するコード例です。`[0-9]`は数字を表し、`+`は1回以上の繰り返しを表します。

```
let text = "私の電話番号は080-1234-5678です。";
let regex = Regex::new(r"[0-9]{3}-[0-9]{4}-[0-9]{4}").unwrap();
let phone_number = regex.find(text).unwrap().as_str();

println!("{}", phone_number); // 080-1234-5678
```

また、正規表現を使用して文字列内の特定の単語を置換することもできます。以下のコード例では、`replace_all`メソッドを使用して文字列内の`rust`を`Rust🦀`に置換しています。

```
let text = "I love rust programming!";
let regex = Regex::new(r"rust").unwrap();
let replaced_text = regex.replace_all(text, "Rust🦀");

println!("{}", replaced_text); // I love Rust🦀 programming!
```

## 深堀り

正規表現を使用する際によく使われる記号や特殊な使い方など、より詳しい情報を知りたい方は、以下のリンクを参考にしてください。

- [正規表現入門](https://www.javadrive.jp/regex/)
- [正規表現チートシート](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [正規表現入門 (Rust)](https://tourofrust.com/20_ja.html)

## 関連リンク

- [Rustクイックスタート](https://www.rust-lang.org/ja/learn/get-started)
- [Rustプログラミング言語](https://doc.rust-lang.org/book/)
- [Rustクレート一覧](https://crates.io/)