---
title:                "Rust: JSONを使用したプログラミング"
simple_title:         "JSONを使用したプログラミング"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/working-with-json.md"
---

{{< edit_this_page >}}

## なぜJSONを取り扱うか

JSONは現代のウェブ開発にとって重要な役割を果たしています。ウェブアプリケーションやモバイルアプリケーションでは、サーバーとの通信でデータをやりとりする必要があります。このとき、データの形式としてよく使われるのがJSONです。データの取得や送信には、JSONを扱えるようにする必要があるため、JSONを学ぶことはとても重要です。

## 作り方

Rustでは、serdeというcrateを使ってJSONを扱うことができます。まずは、プロジェクトにserdeを追加し、次にJSONとのシリアライズ・デシリアライズを実装するための構造体を作成します。以下のコードは、タスクのリストをJSON形式で保存し、取得する例です。

```rust
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize)]
struct Task {
    id: u32,
    title: String,
    completed: bool,
}

fn main() {
    // タスクのリスト
    let tasks = vec![
        Task {
            id: 1,
            title: String::from("ミーティングの準備"),
            completed: false,
        },
        Task {
            id: 2,
            title: String::from("プレゼンの作成"),
            completed: true,
        },
    ];

    // シリアライズ
    let json = serde_json::to_string(&tasks).expect("JSONの作成に失敗しました。");

    // デシリアライズ
    let deserialized_tasks: Vec<Task> = serde_json::from_str(&json).expect("JSONのパースに失敗しました。");

    // データの表示
    println!("{}", json);
    println!("{:?}", deserialized_tasks);
}
```

実行結果は以下のようになります。

```bash
[{"id":1,"title":"ミーティングの準備","completed":false},{"id":2,"title":"プレゼンの作成","completed":true}]

[Task { id: 1, title: "ミーティングの準備", completed: false }, Task { id: 2, title: "プレゼンの作成", completed: true }]
```

## 深堀り

以上は基本的な使い方ですが、実際の開発ではより複雑なJSON形式のデータを扱うことがあります。そのような場合、serdeのカスタムデシリアライザを作成することで、より詳細な設定が可能になります。また、JSON以外のデータ形式にも対応できるように、serdeの支援を受けながら自分でデシリアライザを作成することもできます。

## また見る

- [serde公式ドキュメント](https://serde.rs/)
- [RustでJSONを扱う方法](https://dev.classmethod.jp/articles/rust-json-serialize-deserialize/)