---
title:                "Rust: Yamlを使用する方法"
simple_title:         "Yamlを使用する方法"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜRustでYAMLを使うのか

Rustは高速で安全性が高く、プログラムのバグを防ぐことができる優れたプログラミング言語です。そのため、YAMLファイルの読み書きなど、データ処理を安全かつ効率的に行うことができます。

## 方法

```Rust
extern crate yaml_rust; // ライブラリをインポート
use yaml_rust::{YamlLoader, Yaml}; // Yamlファイルをロードするための関数を使う

fn main() {
    let yaml_str = "
    name: John
    age: 30
    hobbies:
        - reading
        - hiking
    "; // YAML形式の文字列を定義

    let docs = YamlLoader::load_from_str(&yaml_str).unwrap(); // YAMLファイルをパースして、構造体として読み込む

    let doc = &docs[0]; // 最初のドキュメントを取得
    let name = doc["name"].as_str().unwrap(); // nameキーの値を取得し、文字列に変換
    let age = doc["age"].as_i64().unwrap(); // ageキーの値を取得し、整数に変換
    let hobbies = doc["hobbies"].as_vec().unwrap(); // hobbiesキーの値を取得し、ベクターに変換

    println!("My name is {}, I am {} years old.", name, age); // 結果を出力

    println!("Here are my hobbies:");
    for h in hobbies {
        println!("- {}", h.as_str().unwrap()); // ループして、各趣味を出力
    }
}
```

コードの実行結果：

```
My name is John, I am 30 years old.
Here are my hobbies:
- reading
- hiking
```

## ディープダイブ

YAMLファイルを読み書きする際には、YAMLライブラリが提供するさまざまな方法を知ることが重要です。例えば、Yamlファイルを作成するには `yaml_rust::YamlEmitter` を使用し、Yamlファイルを操作するには `yaml_rust::YamlValue` を使用します。また、Rustのパターンマッチングを活用することで、Yamlファイルの特定の値を素早く取得することもできます。

## 他にも見てみる

- [Rust公式ドキュメント](https://www.rust-lang.org/learn)
- [YAMLライブラリのリポジトリ](https://github.com/yaml-rust/yaml-rust)
- [YAML構文の詳細](https://yaml.org/spec/1.2/spec.html)