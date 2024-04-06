---
date: 2024-01-26 03:42:28.700149-07:00
description: "\u65B9\u6CD5: \u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\
  \u524A\u9664\u3059\u308B\u6642\u3001\u306A\u305C\u305D\u308C\u304C\u5358\u306B `.replace(\"\
  \\\"\", \"\")`\u2026"
lastmod: '2024-04-05T22:50:55.760696-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\
  \u308B\u6642\u3001\u306A\u305C\u305D\u308C\u304C\u5358\u306B `.replace(\"\\\"\"\
  , \"\")` \u3067\u306A\u3044\u306E\u304B\u7591\u554F\u306B\u601D\u3046\u304B\u3082\
  \u3057\u308C\u307E\u305B\u3093\u3002\u521D\u671F\u306B\u306F\u3001\u30C6\u30AD\u30B9\
  \u30C8\u306E\u53D6\u308A\u6271\u3044\u304C\u3042\u307E\u308A\u7D71\u4E00\u3055\u308C\
  \u3066\u304A\u3089\u305A\u3001\u7570\u306A\u308B\u30B7\u30B9\u30C6\u30E0\u304C\u30C6\
  \u30AD\u30B9\u30C8\u3092\u4FDD\u5B58\u304A\u3088\u3073\u9001\u4FE1\u3059\u308B\u7570\
  \u306A\u308B\u65B9\u6CD5\u3092\u4F7F\u7528\u3057\u3066\u3044\u3066\u3001\u7279\u6B8A\
  \u6587\u5B57\u306E\u305F\u3081\u306E\u4F55\u3089\u304B\u306E\u300C\u30A8\u30B9\u30B1\
  \u30FC\u30D7\u30B7\u30FC\u30B1\u30F3\u30B9\u300D\u304C\u3088\u304F\u3042\u308A\u307E\
  \u3057\u305F\u3002Rust\u306E `trim_matches` \u30E1\u30BD\u30C3\u30C9\u306F\u3088\
  \u308A\u591A\u69D8\u3067\u3042\u308A\u3001\u3069\u306E\u6587\u5B57\u3092\u30C8\u30EA\
  \u30E0\u3059\u308B\u304B\u3001\u6587\u5B57\u5217\u306E\u958B\u59CB\u90E8\u5206\uFF08\
  \u30D7\u30EC\u30D5\u30A3\u30C3\u30AF\u30B9\uFF09\u3001\u7D42\u4E86\u90E8\u5206\uFF08\
  \u30B5\u30D5\u30A3\u30C3\u30AF\u30B9\uFF09\u3001\u3042\u308B\u3044\u306F\u4E21\u5074\
  \u304B\u3089\u30C8\u30EA\u30E0\u3059\u308B\u304B\u3092\u6307\u5B9A\u3067\u304D\u307E\
  \u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

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
