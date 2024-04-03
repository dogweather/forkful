---
date: 2024-01-26 03:42:28.700149-07:00
description: "\u65B9\u6CD5: ."
lastmod: '2024-03-13T22:44:41.802102-06:00'
model: gpt-4-0125-preview
summary: .
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
