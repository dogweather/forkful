---
title:                "「現在の日付を取得する」"
html_title:           "Elixir: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
日付を取得するのは、多くのプログラムでよく必要とされるタスクです。現在の日付を取得することで、アプリケーションの動作を時間に基づいて制御したり、日付ベースのデータを処理したりすることができます。

## 方法
```Elixir
Date.utc_today()

Date.today()

DateTime.utc_now()

DateTime.utc_now!("Europe/Paris")
```
これらのコードは、それぞれ協定世界時（UTC）の現在の日付、現地時刻での現在の日付、UTCの現在時刻、および特定のタイムゾーン（ここではパリ）のUTCの現在時刻を返します。詳細な使用法やさまざまなオプションについては、公式ドキュメントを参照してください。

## 深層
Elixirでは、Date、Time、DateTimeという3つのモジュールを使用して日付と時間を取得します。これらはすべてElixirの標準ライブラリで提供され、日付と時間に関するさまざまな機能を提供します。また、Elixirでは日付と時間を操作するためのさまざまなライブラリも提供されています。

## 参考リンク
- [Elixir公式ドキュメント](https://hexdocs.pm/elixir/Date.html)
- [Elixir Dateライブラリ](https://hex.pm/packages/date)
- [Elixir Timexライブラリ](https://hex.pm/packages/timex)