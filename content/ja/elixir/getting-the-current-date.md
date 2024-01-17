---
title:                "現在の日付を取得する"
html_title:           "Elixir: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何か & なんで？

最新の日付を手に入れることは、プログラマーにとって非常に重要です。日付は、データベースの操作やログの記録など、さまざまなプログラムにおいて必要な要素です。また、日付を取得することで、利用者にとってより使いやすい日付形式を提供することもできます。

## 使い方：

Elixirでは、`Date.utc_today/0`という関数を使用することで、現在の日付を取得することができます。以下は、Elixirのコードブロックを使用した例です。

```Elixir
today = Date.utc_today()
IO.puts("Today is #{today}")
```

出力は以下のようになります。

```Elixir
Today is 2021-08-12
```

## 詳細を深く掘り下げる：

日付を取得する方法は、言語やライブラリによって異なります。Elixirの場合、DateTimeモジュールを使用することで、現在の日付や時刻をより詳細に取得することができます。また、外部のライブラリを使用することで、より柔軟な日付の取得が可能になります。

## 関連情報：

- [Elixir公式ドキュメント: DateTimeモジュール](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir公式ドキュメント: Dateモジュール](https://hexdocs.pm/elixir/Date.html)
- [Elixir公式ドキュメント: Calendarモジュール](https://hexdocs.pm/elixir/Calendar.html)