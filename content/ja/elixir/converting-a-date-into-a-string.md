---
title:                "Elixir: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換するのに気を取られなければならない理由はたくさんありますが、一つは日付をデータベースや外部アプリケーションとやり取りする必要があるからです。それに加えて、ユーザーにとってわかりやすい形式で日付を表示したいというニーズもあります。

## 方法

```Elixir
defmodule DateConverter do
  def convert(date) do
    date |> NaiveDateTime.to_date |> Date.to_iso8601
  end
end
```

この例では、最初に `NaiveDateTime` によって与えられた日時を `Date` に変換し、その後 `to_iso8601` 関数を使用してISO 8601形式の文字列に変換しています。結果は次のようになります：

```Elixir
DateConverter.convert(~D[2022-01-01])

"2022-01-01"
```

## ディープダイブ

上記の方法は、一般的な日付の変換方法ですが、最適な方法ではありません。日付と時刻を取り扱うのに最適化された `DateTime` モジュールや、異なるタイムゾーンを考慮する `Timex` ライブラリなど、さまざまなツールがElixirにはあります。また、日付を文字列に変換する際にタイムゾーンを指定することも重要です。詳細は公式ドキュメントを参照してください。

## See Also

- [Elixir DateTimeモジュール](https://hexdocs.pm/elixir/DateTime.html)
- [Timexライブラリ](https://hexdocs.pm/timex/Timex.html)
- [ISO 8601形式についての詳細](https://en.wikipedia.org/wiki/ISO_8601)