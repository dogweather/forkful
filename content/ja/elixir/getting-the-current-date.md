---
title:                "Elixir: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
日付を取得することの意義は、多くのプログラミングタスクにおいて必要不可欠です。例えば、販売アプリケーションでは注文日を追跡するために日付が必要です。また、課金システムでは有効期限の計算に日付が重要です。Elixirを学ぶ上で、日付を取得する方法は非常に重要です。

## 方法

日付を取得する最も簡単な方法は、`Date.utc_today()`関数を使用することです。この関数は、現在の日付をUTCフォーマットで返します。例えば、以下のコードを実行すると、現在の日付が`{:ok, #Date<2020-10-15>}`というタプルの形式で返されます。

```Elixir
Date.utc_today()
```

また、特定のタイムゾーンで日付を取得する場合は、`Date.now()`関数を使用することもできます。以下のコードでは、日本時間での現在の日付を取得しています。

```Elixir
tz = Timezone.get("Asia/Tokyo")
Date.now(tz)
```

さらに、日付や時刻のフォーマットを変更する必要がある場合は、`DateTime.to_string()`関数を使用することができます。以下のコードでは、現在の日付をyyyy-MM-ddの形式で文字列として返しています。

```Elixir
DateTime.to_string(Date.utc_today(), "yyyy-MM-dd")
```

## ディープダイブ
Elixirでは、日付を取得する方法についてさらに深く学ぶことができます。例えば、Elixirの標準ライブラリには`Calendar`モジュールがあり、日付の演算を行うための多くの関数が用意されています。また、`Timex`ライブラリを使用することで、より高度な日付と時刻の操作が可能になります。

さらに、Elixirの`DateTime`モジュールは、日付や時刻だけでなく、タイムゾーンや曜日などの様々な情報を含んでいます。これにより、より詳細な日付の操作や表示が可能になります。

## See Also
- [Elixir公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#dates-and-times)
- [Elixirのタイムゾーンおよび時刻操作のためのライブラリ - Timex](https://github.com/bitwalker/timex)
- [TDさんのElixirハンズオン冊子](https://elixir-hands-on.github.io/slides/4-date-time.html)