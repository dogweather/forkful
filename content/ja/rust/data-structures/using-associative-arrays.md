---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:09.155085-07:00
description: "\u65B9\u6CD5\uFF1A Rust\u3067\u306F\u3001`std::collections`\u30E2\u30B8\
  \u30E5\u30FC\u30EB\u306E`HashMap`\u578B\u304C\u9023\u60F3\u914D\u5217\u306E\u6A5F\
  \u80FD\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u304C\u305D\
  \u308C\u3089\u3092\u4F7F\u7528\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.714745-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法：
Rustでは、`std::collections`モジュールの`HashMap`型が連想配列の機能を提供しています。これがそれらを使用する方法です：

```Rust
use std::collections::HashMap;

fn main() {
    // 新しいHashMapを作成
    let mut scores = HashMap::new();

    // 値を挿入
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 50);

    // 値にアクセス
    let team_name = String::from("Blue");
    if let Some(score) = scores.get(&team_name) {
        println!("チームBlueのスコア: {}", score); // 出力: チームBlueのスコア: 10
    }

    // 値を更新
    scores.entry(String::from("Blue")).and_modify(|e| *e += 5);

    // キーと値のペアを繰り返し処理
    for (key, value) in &scores {
        println!("{}: {}", key, value); // 出力: Blue: 15, Yellow: 50
    }
}
```

## 深掘り
Rustの`HashMap`は、キーを値にマッピングするためにハッシュ関数を使用し、これによりデータの迅速な取得が可能になります。しかし、この効率性にはコストが伴います：ハッシュマップはその要素の順序を保持しません。これは、Python（`dict`）やRubyなど、最近のバージョンでは挿入順を機能として保持する他の連想配列の実装とは対照的です。キーと値のペアの順序が重要な使用例の場合、Rust開発者は順序を保持するが、`HashMap`に比べて挿入と取得が遅くなる可能性がある`std::collections`モジュールの`BTreeMap`を使用することを検討するかもしれません。結局のところ、`HashMap`と`BTreeMap`の選択は、順序とパフォーマンスに関する特定の要件に依存します。
