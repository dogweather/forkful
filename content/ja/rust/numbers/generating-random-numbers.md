---
date: 2024-01-27 20:35:37.064250-07:00
description: "\u65B9\u6CD5: Rust\u306F\u4E71\u6570\u751F\u6210\u306B\u5916\u90E8\u30AF\
  \u30EC\u30FC\u30C8\u3092\u4F9D\u5B58\u3057\u3066\u304A\u308A\u3001`rand`\u304C\u6700\
  \u3082\u4E00\u822C\u7684\u306B\u4F7F\u7528\u3055\u308C\u3066\u3044\u307E\u3059\u3002\
  \u4E71\u6570\u751F\u6210\u3092\u958B\u59CB\u3059\u308B\u306B\u306F\u3001\u307E\u305A\
  `Cargo.toml`\u30D5\u30A1\u30A4\u30EB\u306B`rand`\u3092\u8FFD\u52A0\u3059\u308B\u5FC5\
  \u8981\u304C\u3042\u308A\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.815528-06:00'
model: gpt-4-0125-preview
summary: "Rust\u306F\u4E71\u6570\u751F\u6210\u306B\u5916\u90E8\u30AF\u30EC\u30FC\u30C8\
  \u3092\u4F9D\u5B58\u3057\u3066\u304A\u308A\u3001`rand`\u304C\u6700\u3082\u4E00\u822C\
  \u7684\u306B\u4F7F\u7528\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u4E71\u6570\u751F\
  \u6210\u3092\u958B\u59CB\u3059\u308B\u306B\u306F\u3001\u307E\u305A`Cargo.toml`\u30D5\
  \u30A1\u30A4\u30EB\u306B`rand`\u3092\u8FFD\u52A0\u3059\u308B\u5FC5\u8981\u304C\u3042\
  \u308A\u307E\u3059\uFF1A."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法:
Rustは乱数生成に外部クレートを依存しており、`rand`が最も一般的に使用されています。乱数生成を開始するには、まず`Cargo.toml`ファイルに`rand`を追加する必要があります：

```toml
[dependencies]
rand = "0.8.5"
```

次に、Rustコード内で`rand`を使用して乱数を生成することができます。次はランダムな整数と浮動小数点数を生成する例です：

```rust
use rand::{Rng, thread_rng};

fn main() {
    let mut rng = thread_rng();
    
    // 1から10の間のランダムな整数を生成
    let random_int: i32 = rng.gen_range(1..=10);
    println!("Random Integer: {}", random_int);
    
    // 0.0から1.0の間のランダムな浮動小数点数を生成
    let random_float: f64 = rng.gen::<f64>();
    println!("Random Float: {}", random_float);
}
```

サンプル出力は以下のようになるかもしれません：

```plaintext
Random Integer: 7
Random Float: 0.9401077112175732
```

プログラムを再実行すると異なる値が生成されることに注意してください。

## 詳細な解説
Rustでの乱数生成は、`rand`やその依存関係である`getrandom`を介したもので、オペレーティングシステムの施設やアルゴリズム生成器を幅広く抽象化しています。歴史的に、コンピューティングにおけるランダム性は単純で予測可能なアルゴリズムから複雑で暗号学的に安全な方法へと進化してきました。Rustは、必要なランダム性の質とパフォーマンスに応じて様々なジェネレーターによって支えられることが可能なプラグ可能な`Rng`トレイトを通じて、この進化を包含しています。

ほとんどのアプリケーションでは、`rand`とシステムのRNGに依存することで、シンプルさとエントロピーの良いバランスが得られます。しかし、暗号化アプリケーションに対しては、`rand`はシード生成に`getrandom`を委ねますが、これはOS固有のメカニズム（例えば、Unix系システムでは`/dev/urandom`）に依存し、暗号学的に安全なランダム性を保証します。

一方、`rand`で満たされない特定のニーズがある場合、他のクレートを探索したり、数学モデルに基づいたカスタムジェネレーターを実装することも一つの方法です。とはいえ、大多数の使用例において、`rand`及びそのエコシステムは、Rustアプリケーションに効率的かつ簡単に統合できる堅牢な解決策を提供しています。
