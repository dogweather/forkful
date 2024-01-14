---
title:                "Gleam: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

機能を使用する理由は、特定のパターンに一致する文字を削除することで、テキストデータをきれいにするためです。

## 方法

以下のコード例を使用して、Gleamで文字をパターンに一致する方法を説明します。コードブロックは、 ```Gleam ... ```で囲まれています。

```Gleam
// テキストデータを定義する
let message = "今日はいい天気です。雲がありますが、明日の天気の準備ができています。"

// "天気"という文字列を含む箇所を削除する
let cleaned_message =
  String.trim(message, Pattern.regex("天気"))

// 出力
"今日はいいです。がありますが、明日のの準備ができています。"
```

## 深いダイブ

文字をパターンに一致させるために使用される正規表現の詳細について説明します。この機能を使用する際には、正しいパターンを指定することが重要です。

## See Also

- 正規表現の基本概念についての記事：https://techacademy.jp/magazine/15848
- Gleamの公式ドキュメント：https://gleam.run/