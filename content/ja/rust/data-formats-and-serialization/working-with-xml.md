---
date: 2024-01-26 04:36:11.431000-07:00
description: "\u65B9\u6CD5 Rust\u3067XML\u3092\u6271\u3046\u306B\u306F\u3001`xml-rs`\u306E\
  \u3088\u3046\u306A\u30AF\u30EC\u30FC\u30C8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\
  `Cargo.toml`\u306B`xml-rs = \"0.8\"`\u3092\u8FFD\u52A0\u3057\u3066\u30A4\u30F3\u30B9\
  \u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u30B7\u30F3\u30D7\u30EB\
  \u306AXML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.138075-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5 Rust\u3067XML\u3092\u6271\u3046\u306B\u306F\u3001`xml-rs`\u306E\
  \u3088\u3046\u306A\u30AF\u30EC\u30FC\u30C8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\
  `Cargo.toml`\u306B`xml-rs = \"0.8\"`\u3092\u8FFD\u52A0\u3057\u3066\u30A4\u30F3\u30B9\
  \u30C8\u30FC\u30EB\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u30B7\u30F3\u30D7\u30EB\
  \u306AXML\u3092\u30D1\u30FC\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 方法
RustでXMLを扱うには、`xml-rs`のようなクレートを使用します。`Cargo.toml`に`xml-rs = "0.8"`を追加してインストールします。以下はシンプルなXMLをパースする方法です：

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("開始: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("テキスト: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("終了: {}", name);
            }
            Err(e) => {
                println!("エラー: {}", e);
            }
            _ => {}
        }
    }
}
```

出力：
```
開始: book
開始: title
テキスト: Rust in Action
終了: title
開始: author
テキスト: Tim McNamara
終了: author
開始: year
テキスト: 2021
終了: year
終了: book
```
このコードはXMLをストリーム読み込みし、開始要素と終了要素、テキストデータを処理し、各ステップをログに記録します。

## 深掘り
XMLは技術年数においてシニアで、90年代後半にウェブ用に作られました。その設計は可読性（機械と人の両方のため）と広範な自己記述データを促進します。

代替品は？確かに、JSONはより軽量で騒がしくないウェブAPIのための現代の定番です。一方、YAMLはそのクリーンなレイアウトで設定のためのファンを獲得しました。しかし、XMLは大規模なインフラストラクチャがその背中に構築されているため、すぐにはどこにも行きません。

内部的には、RustのXML解析はイテレーターパターンに依存しており、メモリ使用量を低く保ちながらパフォーマンスを高めています。JSONの取り扱いに慣れている人には朗報である、よりserdeのような経験を提供するクレート、例えば`serde-xml-rs`が見つかります。

## 参照
RustとXMLについてはこちらを参照：
- Rustのserde互換性のための`serde-xml-rs`：[https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- 公式のRustドキュメント（ブラッシュアップしても損はない）：[https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
