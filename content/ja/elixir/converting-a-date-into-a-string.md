---
title:                "Elixir: 「日付を文字列に変換する」"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Elixirプログラミングのためのデートを文字列に変換する

## なぜ

デートを文字列に変換することは、日付データをプログラムで簡単に操作できるようにするために非常に便利です。他のデータ型に比べて、日付は特別なフォーマットや構造を持っているため、このような変換が必要です。

## 方法

```Elixir
Date.to_string(Date.utc_today())
```

このコードを実行すると、今日の日付が文字列に変換されます。

出力は以下のようになります。

```
"2021-10-05"
```

日付を変換する際には、Date.to_string関数を使います。この関数は日付オブジェクトを文字列に変換し、指定されたフォーマットに従ってデータを整形することができます。詳しくは、Elixirの公式ドキュメントを参照してください。

## 詳細を掘り下げる

日付を文字列に変換する際には、変換する際に考慮すべきいくつかの要素があります。

まず、使用するフォーマットについて考える必要があります。Elixirでは、"YYYY-MM-DD"のような形式が一般的ですが、他にも様々なフォーマットがあります。きちんと設定しておくことで、プログラムでの日付の表示方法を統一できます。

また、タイムゾーンについても考慮する必要があります。Elixirでは、デフォルトで「ローカルタイムゾーン」が使用されます。これは、コンピューターの設定に基づいています。しかし、特定のタイムゾーンを指定することもできます。例えば、UTCタイムゾーンを使用して欧州の開発者と協力する場合などに便利です。Date.to_string関数には、オプション引数としてタイムゾーンを指定することができます。

## 参考

[Official Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)

[Understanding Time and Time Zones in Elixir](https://www.mattlean.com/elixir-time-timezones/)

[Handling Time and Time Zones in Elixir - Aleksandar Simović](https://medium.com/alexandarsimovic/handling-time-and-time-zones-in-elixir-28f4f83d8562)

### 参考文献

- [Official Elixir Date module documentation](https://hexdocs.pm/elixir/Date.html)
- [Understanding Time and Time Zones in Elixir](https://www.mattlean.com/elixir-time-timezones/)
- [Handling Time and Time Zones in Elixir - Aleksandar Simović](https://medium.com/alexandarsimovic/handling-time-and-time-zones-in-elixir-28f4f83d8562)