---
title:                "「JSONを扱うこと」"
html_title:           "Rust: 「JSONを扱うこと」"
simple_title:         "「JSONを扱うこと」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-json.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
JSONとは、データの構造化されたフォーマットの一つであり、プログラマーがデータの受け渡しや保存に使用することができます。JSONを使用することで、データを柔軟に管理することができるため、多様なアプリケーションやシステムでよく使われています。

## やり方：
以下のコードブロック内には、JSONを作成・アクセスする方法の例とその出力が示されています。

```Rust
use serde_json::{Value, json}; // クレートのインポート

fn main() {
    // JSONを作成
    let mut my_json = json!({
        "name": "John",
        "age": 30,
        "hobbies": ["reading", "gaming", "cooking"]
    });

    // JSONの値にアクセス
    println!("Name: {}", my_json["name"]); // 出力：Name: John
    println!("Age: {}", my_json["age"]); // 出力：Age: 30

    // 新しいキーと値を追加
    my_json["favorite_color"] = json!("blue");

    // JSONの値を変更
    my_json["age"] = json!(31);

    // インデントを使用して美しく表示
    println!("JSON: {}", serde_json::to_string_pretty(&my_json).unwrap());
    /*
    出力：
    {
        "name": "John",
        "age": 31,
        "hobbies": ["reading", "gaming", "cooking"],
        "favorite_color": "blue"
    }
    */
}
```

## 詳細を調べる：
JSONは、データの保存や交換のために生み出された軽量なフォーマットです。他にもXMLやYAMLといったフォーマットが存在し、それぞれ特有の特性がありますが、JSONはシンプルで理解しやすいため、広く使われています。Rustでは、serde_jsonというクレートを使用してJSONを扱うことができます。

## 関連リンク：
- [RustにおけるJSONのドキュメント](https://docs.serde.rs/serde_json/)
- [JSONの歴史と背景についての記事](https://www.json.org/json-ja.html)
- [JSONの代替フォーマットとしてのXMLとYAMLの比較](https://medium.com/@chrisalbon/why-json-isnt-really-xml-javascript-object-notation-vs-extensible-markup-language-301692b25c0c)