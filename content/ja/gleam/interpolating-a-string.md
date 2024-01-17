---
title:                "文字列の補間"
html_title:           "Gleam: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
文字列のインポーティングが何であるかと、プログラマーがなぜそれを行うのかを説明します。

文字列のインポーティングとは、あなたが既存のストリングを組み合わせたり、変数を埋め込んだりして、新しい文字列を作成することです。プログラマーはこれを行うことで、コードをより柔軟にし、繰り返し作業を減らし、プログラムの実行をより効率的にすることができます。

## 使い方：
以下のコードブロック内にコーディングの例とサンプル出力を示します。Gleamの文法に従ってください。

```
Gleam
let name = "山田";
let greeting = "こんにちは、{name}さん！";

let output = interpolate("こんにちは、{name}さん！", name);

assert.equal(output, greeting); // 出力は "こんにちは、山田さん！" となります。
```

## 深堀り：
文字列のインポーティングには、以前は文字列を結合することが一般的でした。しかし、これは繰り返し作業を伴い、バグの原因となりやすいものでした。インポーティングを使用することで、より効率的に文字列を生成できるようになりました。

文字列のインポーティングの代替方法として、テンプレートリテラルやフォーマット関数があります。しかし、Gleamのインポーティングはより安全であり、コンパイル時にエラーを検出することができます。

文字列のインポーティングは、Gleamで使用できる様々な型に対応しています。これにより、複雑な文字列を生成する際にも便利に使用することができます。

## 関連情報：
- [Gleamドキュメント- 文字列のインポーティング](https://gleam.run/documentation/current/string-interpolation)
- [Gleamコミュニティフォーラム](https://forum.gleam.run/t/string-interpolation-in-gleam/238)
- [「プログラミング入門」- インポーティングとは？](https://programmingintro.com/concepts/string-interpolation)