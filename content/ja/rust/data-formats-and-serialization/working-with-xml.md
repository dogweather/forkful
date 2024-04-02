---
date: 2024-01-26 04:36:11.431000-07:00
description: "XML\u306F\u3001\u300CeXtensible Markup Language\u300D\u306E\u7565\u3067\
  \u3001JSON\u306E\u5197\u9577\u306A\u3044\u3068\u3053\u306E\u3088\u3046\u306A\u3082\
  \u306E\u3067\u3059\u3002\u907A\u7523\u30B7\u30B9\u30C6\u30E0\u3001\u30A8\u30F3\u30BF\
  \u30FC\u30D7\u30E9\u30A4\u30BA\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u3001\u307E\u305F\
  \u306FJSON\u30D0\u30F3\u30C9\u30EF\u30B4\u30F3\u3092\u30B9\u30AD\u30C3\u30D7\u3057\
  \u305FAPI\u3092\u6271\u3046\u969B\u306BXML\u3068\u683C\u95D8\u3059\u308B\u3053\u3068\
  \u306B\u306A\u308A\u307E\u3059\u3002XML\u304C\u305D\u306E\u5730\u4F4D\u3092\u5B88\
  \u308B\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u306F\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.856737-06:00'
model: gpt-4-0125-preview
summary: "XML\u306F\u3001\u300CeXtensible Markup Language\u300D\u306E\u7565\u3067\u3001\
  JSON\u306E\u5197\u9577\u306A\u3044\u3068\u3053\u306E\u3088\u3046\u306A\u3082\u306E\
  \u3067\u3059\u3002\u907A\u7523\u30B7\u30B9\u30C6\u30E0\u3001\u30A8\u30F3\u30BF\u30FC\
  \u30D7\u30E9\u30A4\u30BA\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u3001\u307E\u305F\u306F\
  JSON\u30D0\u30F3\u30C9\u30EF\u30B4\u30F3\u3092\u30B9\u30AD\u30C3\u30D7\u3057\u305F\
  API\u3092\u6271\u3046\u969B\u306BXML\u3068\u683C\u95D8\u3059\u308B\u3053\u3068\u306B\
  \u306A\u308A\u307E\u3059\u3002XML\u304C\u305D\u306E\u5730\u4F4D\u3092\u5B88\u308B\
  \u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u306F\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 何となぜ？
XMLは、「eXtensible Markup Language」の略で、JSONの冗長ないとこのようなものです。遺産システム、エンタープライズソフトウェア、またはJSONバンドワゴンをスキップしたAPIを扱う際にXMLと格闘することになります。XMLがその地位を守るデータ交換には不可欠です。

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
