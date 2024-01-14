---
title:                "Gleam: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに対する動機を知りたいですか？それは、プログラミング言語での文字列処理において、非常に一般的なタスクだからです。文字列の長さを取得することで、文字列を適切に操作し、データを結合したり分割したりすることができます。

## 手順

文字列の長さを取得するには、Gleamの組み込み関数である`String.length`を使用します。まずは、文字列を定義してみましょう。

```Gleam
let message = "こんにちは世界"
```

ここでは、日本語で「こんにちは世界」というメッセージが文字列として定義されています。そして、`String.length`を使用して、文字列の長さを取得します。

```Gleam
String.length(message)
```

このコードを実行すると、`10`という出力が得られます。これは、「こんにちは世界」という文字列が10文字であることを示しています。

## 深い掘り下げ

文字列の長さを取得する方法は、非常にシンプルですが、内部ではどのように処理されているのでしょうか？実際には、Gleamの標準ライブラリにある`String`モジュールの中に`length`関数が定義されています。

この関数は、文字列をバイトの配列として処理し、その長さを計算して返します。また、日本語や他の多言語をサポートするために、UTF-8という文字コードを使用しています。

さらに、`length`関数は、Gleamのパターンマッチングと再帰処理を使用して、効率的に文字列の長さを求めるように最適化されています。

## さらに見る

- [Gleam公式ドキュメント - Stringモジュール](https://gleam.run/modules/string.html)
- [UTF-8とは？](https://ja.wikipedia.org/wiki/UTF-8)
- [Gleam公式チュートリアル - 文字列操作](https://gleam.run/tour/strings.html)