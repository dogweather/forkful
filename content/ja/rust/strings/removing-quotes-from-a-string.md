---
date: 2024-01-26 03:42:28.700149-07:00
description: "Rust\u3067\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\
  \u9664\u3059\u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u306E\
  \u5468\u308A\u306B\u3042\u308B\u4F59\u5206\u306A\u5F15\u7528\u7B26\u6587\u5B57\u3092\
  \u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u305D\u308C\u3092\u884C\u3046\u306E\u306F\u3001\
  \u6587\u5B57\u5217\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u3057\u305F\u308A\
  \u3001\u6A19\u6E96\u5316\u3057\u305F\u308A\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\
  \u5834\u5408\u3001\u4F8B\u3048\u3070\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u306E\u30C7\
  \u30FC\u30BF\u89E3\u6790\u5F8C\u3084\u3001\u5F15\u7528\u7B26\u304C\u554F\u984C\u3068\
  \u306A\u308B\u304B\u4F59\u8A08\u3067\u3042\u308B\u4ED6\u306E\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u306B\u6E96\u5099\u3059\u308B\u6642\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.802102-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\
  \u3059\u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u306E\u5468\
  \u308A\u306B\u3042\u308B\u4F59\u5206\u306A\u5F15\u7528\u7B26\u6587\u5B57\u3092\u53D6\
  \u308A\u9664\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u304C\u305D\u308C\u3092\u884C\u3046\u306E\u306F\u3001\u6587\
  \u5B57\u5217\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u3057\u305F\u308A\u3001\
  \u6A19\u6E96\u5316\u3057\u305F\u308A\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\u5834\
  \u5408\u3001\u4F8B\u3048\u3070\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u306E\u30C7\u30FC\
  \u30BF\u89E3\u6790\u5F8C\u3084\u3001\u5F15\u7528\u7B26\u304C\u554F\u984C\u3068\u306A\
  \u308B\u304B\u4F59\u8A08\u3067\u3042\u308B\u4ED6\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u306B\u6E96\u5099\u3059\u308B\u6642\u3067\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 何となぜ？

Rustで文字列から引用符を削除するとは、テキストデータの周りにある余分な引用符文字を取り除くことを意味します。プログラマーがそれを行うのは、文字列をクリーンアップしたり、標準化したりする必要がある場合、例えばファイルからのデータ解析後や、引用符が問題となるか余計である他のフォーマットに準備する時です。

## 方法:

```Rust
fn remove_quotes(s: &str) -> String {
    s.trim_matches(|c| c == '\"' || c == '\'').to_string()
}

fn main() {
    let quoted_str = "\"Hello, Rustaceans!\"";
    let cleaned_str = remove_quotes(quoted_str);
    println!("{}", cleaned_str);
    // 出力: Hello, Rustaceans!
}
```

時々、このように混合引用符を含む文字列があります:

```Rust
fn main() {
    let mixed_quoted = "'Rust says: \"Hello, World!\"'";
    let cleaned_str = remove_quotes(mixed_quoted);
    println!("{}", cleaned_str);
    // 出力: Rust says: "Hello, World!"
}
```

ここでは、最も外側の単一引用符のみが削除されます。

## 詳細分析

文字列から引用符を削除する時、なぜそれが単に `.replace("\"", "")` でないのか疑問に思うかもしれません。初期には、テキストの取り扱いがあまり統一されておらず、異なるシステムがテキストを保存および送信する異なる方法を使用していて、特殊文字のための何らかの「エスケープシーケンス」がよくありました。Rustの `trim_matches` メソッドはより多様であり、どの文字をトリムするか、文字列の開始部分（プレフィックス）、終了部分（サフィックス）、あるいは両側からトリムするかを指定できます。

もちろん、代替方法があります。Regexは、複雑なパターンに一致できる文字列操作のための強力なツールであり、ただ引用符を取り除くだけには過剰かもしれません。`trim_in_place` のようなライブラリは、新しい `String` オブジェクトを作成するオーバーヘッドなしで、その場でトリムを提供でき、パフォーマンスが重要なアプリケーションにとって望ましいかもしれません。

内部では、`trim_matches` は実際には文字列の両端から文字を反復処理し、提供されたパターンに一致しない文字が見つかるまでチェックします。それは行っていることに対して効率的ですが、Unicodeスカラー値で作業していることに常に注意してください。文字列がマルチバイトUnicode文字を含む可能性がある場合、それらを分割することについて心配する必要はありません。

## 参照

- 文字列操作に関するRustのドキュメント: https://doc.rust-lang.org/book/ch08-02-strings.html
- 複雑なパターン用の `regex` クレート: https://crates.io/crates/regex
- 実用的なコーディングシナリオのための Rust by Example: https://doc.rust-lang.org/stable/rust-by-example/std/str.html
