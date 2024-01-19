---
title:                "未来または過去の日付を計算する"
html_title:           "Elixir: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？

将来や過去の日付を計算するとは、特定の日数を現在の日付に加えたり減じたりすることを指します。これは、予定のリマインダーを設定したり、ファイルの有効期限を判断したりするような機能をプログラムに組み込むために行います。

## 進め方：

Elixir言語では、DateTimeモジュールを利用してこのような操作を実装することが出来ます。以下に具体的なコードを示します：

```Elixir
# 現在の日時を取得
iex> now = DateTime.utc_now
# 5日後の日付を計算
iex> DateTime.add(now, 5*24*60*60, :second)
```

この例では、現在の日時の取得とそれから5日後の日付の計算を行っています。

## 詳細情報：

日付と時間の操作は、Unixエポック（1970年1月1日）以来の秒数をメジャーな単位とし、独自の時間体系を持つことで行われます。ElixirのDateTimeモジュールは、この体系を元に設計されています。

他の手段としては、NaiveDateTimeやDateモジュールも存在します。これらはタイムゾーン情報を持たないため、あくまで時間操作に特化した状況下で用いられます。

計算は、秒単位で行われます。1日は86400秒（24時間*60分*60秒）なので、未来・過去の日付計算は今日からの秒数を加える、または引くことで行われます。

## 関連情報：

- [公式ドキュメント](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir School日付と時間](https://elixirschool.com/jp/lessons/basics/date_time/)
- [Elixirで日付・時間の計算をする](https://qiita.com/7kaji/items/bae8c5d6a907409de12c)