---
title:                "Gleam: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの利点は何でしょうか？このブログポストでは、Gleamプログラミング言語で日付を文字列に変換する方法を詳しく説明します。

## やり方

まず、Gleamで日付を文字列に変換する必要なモジュールをインポートします。

```Gleam
import gleam/time as time
```

次に、現在の日付を取得します。

```Gleam
let date = time.now()
```

そして、日付を文字列に変換します。

```Gleam
let string = time.format(date, "%Y-%m-%d")
```

この場合、変換された文字列は"2021-09-30"となります。

## ディープダイブ

日付を文字列に変換する際、Gleamプログラミング言語では、format関数を使用します。この関数には2つの引数があります。最初の引数は日付オブジェクトであり、2つ目の引数は日付をどのような形式の文字列に変換するかを指定するフォーマット文字列です。

フォーマットされた文字列には、使用できる特殊な文字があります。例えば、"%Y"は4桁の年を表し、"%m"は2桁の月を表します。詳細なフォーマット文字列の一覧は、Gleamの公式ドキュメントを参照してください。

## 詳しくはこちら

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [日付を文字列に変換する方法についての詳細な解説記事](https://example.com/converting-date-to-string)
- [Gleamで日付を扱う方法についてのチュートリアル動画](https://youtube.com/gleam-tutorial/123)

## 関連リンク

- [Gleamを使用したWebアプリケーション開発の方法についてのブログポスト](https://example.com/gleam-web-development)
- [Gleamでのデータベース操作の方法についての公式ドキュメント](https://gleam.run/documentation/database)
- [Gleamコミュニティのフォーラム](https://gleam.discourse.group/)