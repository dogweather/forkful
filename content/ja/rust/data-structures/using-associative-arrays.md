---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:09.155085-07:00
description: "\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\u306F Rustaceans \u304C \"\
  \u30CF\u30C3\u30B7\u30E5\u30DE\u30C3\u30D7\" \u3068\u547C\u3076\u3082\u306E\u306F\
  \u3001\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u3067\u30C7\u30FC\u30BF\u3092\u683C\
  \u7D0D\u3059\u308B\u30B3\u30EC\u30AF\u30B7\u30E7\u30F3\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u72EC\u7279\u306A\u30AD\u30FC\u306B\u57FA\u3065\
  \u3044\u3066\u52B9\u7387\u7684\u306B\u30C7\u30FC\u30BF\u3092\u64CD\u4F5C\u3067\u304D\
  \u308B\u3088\u3046\u306B\u3001\u8FC5\u901F\u306A\u30C7\u30FC\u30BF\u691C\u7D22\u306B\
  \u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.810082-06:00'
model: gpt-4-0125-preview
summary: "\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\u306F Rustaceans \u304C \"\u30CF\
  \u30C3\u30B7\u30E5\u30DE\u30C3\u30D7\" \u3068\u547C\u3076\u3082\u306E\u306F\u3001\
  \u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u3067\u30C7\u30FC\u30BF\u3092\u683C\u7D0D\
  \u3059\u308B\u30B3\u30EC\u30AF\u30B7\u30E7\u30F3\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u72EC\u7279\u306A\u30AD\u30FC\u306B\u57FA\u3065\u3044\
  \u3066\u52B9\u7387\u7684\u306B\u30C7\u30FC\u30BF\u3092\u64CD\u4F5C\u3067\u304D\u308B\
  \u3088\u3046\u306B\u3001\u8FC5\u901F\u306A\u30C7\u30FC\u30BF\u691C\u7D22\u306B\u305D\
  \u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 何となぜ？

連想配列、または Rustaceans が "ハッシュマップ" と呼ぶものは、キーと値のペアでデータを格納するコレクションです。プログラマーは、独特なキーに基づいて効率的にデータを操作できるように、迅速なデータ検索にそれらを使用します。

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
