---
title:                "日付を文字列に変換する"
html_title:           "Elixir: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why 
なぜ日付を文字列に変換するのか？
Elixirにおける日付変換の重要性を説明していきます。

日付を文字列に変換する必要がある理由の一つは、データを保管するためです。たとえば、データベースに日付を保存する場合、日付を文字列に変換してから保存する必要があります。また、日付を文字列として出力する場合も同様です。Elixirでは、日付を文字列に変換するための便利な機能がありますので、今回はその使い方を紹介していきます。

## How To

```Elixir
date = ~D[2021-12-31] # 日付の定義
IO.puts(to_string(date)) # 日付を文字列に変換して出力
```

```
> "2021-12-31"
```

```Elixir
time = ~T[23:59:59] # 時刻の定義
IO.puts(to_string(time, {:time, "{YYYY}-{0M}-{DD} {h}時{m}分{s}秒"})) # フォーマットを指定して日付を文字列に変換し出力
```

```
> "2021-00-31 23時59分59秒"
```

日付や時刻を扱うには、まず日付や時刻を定義する必要があります。上記のように日付は`~D`を使用して定義し、時刻は`~T`を使用して定義することができます。そして、`to_string`関数を使用して日付や時刻を文字列に変換することができます。フォーマットを指定することもでき、日付や時刻を任意の形式に変換することができます。

## Deep Dive
日付を文字列に変換する際、Elixirでは主に二つの関数を使用します。一つは先ほど紹介した`to_string`関数で、日付や時刻の値を文字列に変換します。もう一つは`DateTime.to_iso8601`という関数です。この関数はISO 8601というフォーマットに準拠した文字列に日付や時刻を変換します。

```Elixir
date = ~D[2021-12-31]
IO.puts(to_string(date)) # "2021-12-31"
IO.puts(DateTime.to_iso8601(date)) # "2021-12-31T00:00:00"
```

`to_string`関数はISO 8601に準拠していませんが、フォーマットを指定することでハイフンなしやスラッシュ区切りなど、様々な形式に変換することができます。一方で、`DateTime.to_iso8601`関数は常にISO 8601の形式で日付や時刻を出力するため、特定のフォーマットを指定できません。どちらの関数を使用するかは、使用する場面や状況によって決める必要があります。

## See Also
## 参考リンク

- [Elixirの日付/時刻型を扱う方法](https://fukatsu.tech/elixir-datetime)
- [DateTimeモジュール](https://hexdocs.pm/elixir/DateTime.html)
- [to_string関数](https://hexdocs.pm/elixir/String.html#to_string/1)