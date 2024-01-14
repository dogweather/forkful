---
title:    "Gleam: 標準エラーへの書き込み"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーに書き込むのにエキスパートとしてはなぜお勧めするのでしょうか。言い換えると、標準エラーを利用することによって何を得られるのでしょうか。実際のコード例を交えてお話ししましょう。

## 使い方

Gleamから標準エラーに書き込むための方法を解説します。下記コードブロック内に具体的なコード例と出力結果を示します。 

```Gleam
import gleam/io
let message = "こんにちは、世界！"
Io.err(message)
```

出力結果: `こんにちは、世界！`

## 詳しく見ていく

標準エラーについてさらに詳しく見ていきましょう。標準エラーは通常、メインのプログラムの標準出力とは異なる出力先として使用されます。これにより、エラーメッセージを確実に表示することができます。また、標準出力と同様に、標準エラーもファイルやパイプなどのリダイレクトが可能です。

## 同じような記事を見る

[see_also]

- [Gleam公式ドキュメント: IOモジュール](https://gleam.run/documentation/stdlib/io)
- [標準エラーを理解する](https://qiita.com/moriokar/items/6ebdd2766d717ae809df)
- [エラーメッセージを効果的に出力するテクニック](https://tech.gamewith.co.jp/entry/2018/08/31/155917)

[see_also]