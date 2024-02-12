---
title:                "連想配列の使用"
aliases:
- /ja/rust/using-associative-arrays/
date:                  2024-01-30T19:13:09.155085-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
