---
title:    "Rust: 日付を文字列に変換する"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換するのは、日付を表示したり、ファイル名に使用したりするために非常に便利です。Rustでは、このプロセスを非常に簡単にする便利な機能が用意されています。

## 方法

日付を文字列に変換するには、Rustの標準ライブラリである「DateTime」モジュールを使用します。例えば、2021年4月13日を表す日付を文字列に変換するには、以下のようなコードを使用します。

```Rust
use std::time::SystemTime;
use std::time::Duration;

let now = SystemTime::now();
let date = now - Duration::from_secs(86400);

let date_string = date.format("%Y-%m-%d").to_string();

println!("{}", date_string); // Output: 2021-04-13
```

上記のコードでは、まず「SystemTime::now()」を使用して現在の日付を取得し、その後「Duration::from_secs(86400)」を使用して1日前の日付に変換しています。「date.format()」を使用して、指定したフォーマットに従って日付を文字列に変換し、最後に「to_string()」で文字列に変換しています。このように、Rustでは非常に簡単に日付を文字列に変換することができます。

## ディープダイブ

日付を文字列に変換するには、フォーマットの指定が非常に重要です。Rustでは、「format!」マクロを使用してフォーマットを指定することができます。また、日付を文字列から解析することもできます。詳細は公式ドキュメントをご確認ください。

## 参考リンク

- Rust公式ドキュメント 「Date and Time Formatting」: https://doc.rust-lang.org/std/fmt/trait.Display.html#method.fmt-4
- Rust公式ドキュメント 「Date and Time Parsing」: https://doc.rust-lang.org/std/time/struct.DateTime.html
- Rust日本語ドキュメント 「std::format! マクロ」: https://doc.rust-jp.rs/rust-by-example-ja/std_misc/fmt.html
- Qiita 「Rustで日付を扱う」: https://qiita.com/sirone/items/b8b531ea3c5a21e74234