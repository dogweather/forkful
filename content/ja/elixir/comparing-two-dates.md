---
title:                "Elixir: 「日付の比較」"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

For Japanese readers, 今回のブログでは、Elixirで日付を比較する方法について説明します。

## Why

日付を比較することは、日時データを扱うアプリケーションにとって非常に重要です。特定の日付がある日時より前なのか後なのかを知ることで、正しいデータの扱い方ができます。

## How To

Elixirでは、`Date`モジュールと`DateTime`モジュールを使うことで、日付の比較が簡単になります。以下は、二つの日付（`2021-01-01`と`2021-02-01`）を比較する例です。

```Elixir
# Dateモジュールを使った場合
Date.compare(Date.new(2021, 01, 01), Date.new(2021, 02, 01))

# DateTimeモジュールを使った場合
DateTime.compare(DateTime.new(2021, 01, 01), DateTime.new(2021, 02, 01))
```

このコードを実行すると、以下のような結果が得られます。

```Elixir
-1
```

このように、二つの日付を比較すると、その大小関係に応じて`-1`、`0`、`1`のいずれかの値が返されます。

## Deep Dive

日付を比較する際には、時刻の情報も考慮する必要があります。Elixirでは`DateTime.compare/2`メソッドを使うことで、時刻の情報を含めた比較ができます。また、`Date.compare/2`メソッドでは時刻は無視されます。

さらに、日付の範囲を比較する方法として、`DateTime.between?/3`メソッドがあります。引数に指定した二つの日付の間に、第三引数で指定した日付が含まれるかどうかを判定します。

```
DateTime.between?(DateTime.new(2021, 01, 01), DateTime.new(2021, 02, 01), DateTime.new(2021, 01, 15))
```

このコードを実行すると、`true`が返されます。

## See Also

- [Dateモジュールのドキュメント](https://hexdocs.pm/elixir/Date.html)
- [DateTimeモジュールのドキュメント](https://hexdocs.pm/elixir/DateTime.html)

このように、Elixirを使うと日付の比較が簡単にできます。ぜひ、日時データを扱うアプリケーションを開発する際には、この知識を活用してください。