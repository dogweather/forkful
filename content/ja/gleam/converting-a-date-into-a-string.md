---
title:                "Gleam: 「日付を文字列に変換する」"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する必要があるかもしれません。例えば、データベースに日付を保存する場合や、日付を指定したフォーマットで表示したい場合などです。

## 方法

```
Gleam.Date.to_string(Date.new(2021, 7, 15))
```

上記のように、`Gleam.Date.to_string`関数を使用して日付を文字列に変えることができます。上記のコードを実行すると、`2021-07-15`という文字列が返されます。

## 詳しく学ぶ

日付を文字列に変換するには、まず`Gleam.Date.to_string`関数で日付を文字列に変換できるようにします。その際には、Gleamで標準的に使われているISO 8601形式（YYYY-MM-DD）が使用されます。もし、異なるフォーマットで日付を文字列に変換したい場合は、`Gleam.Date.format`関数を使用することで、指定したフォーマットで日付を文字列に変換できます。

## 参考リンク

- [Gleam 公式ドキュメント](https://gleam.run/getting-started/getting-started)
- [Gleam コミュニティフォーラム](https://forum.gleam.run/)
- [ISO 8601 形式を理解する](https://ja.wikipedia.org/wiki/ISO_8601)