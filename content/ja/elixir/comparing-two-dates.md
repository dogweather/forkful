---
title:                "2つの日付の比較"
html_title:           "Elixir: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何？なぜ？

日付を比較するとは、二つの日付を比べてどちらが大きいか、同じかを確認することです。プログラマーたちは、日付を比較することで、データを正確にソートしたり、特定の日付を見つけることができます。

## 方法：

```Elixir
# 日付を比較するための例
date1 = Date.new(2020, 10, 15)
date2 = Date.new(2020, 12, 25)

# 比較する
date1 < date2 # true
date1 == date2 # false
date1 > date2 # false
```

## 詳しく見る：

日付の比較は、ロングプログラミングの歴史に根ざした重要なトピックです。他のプログラミング言語では、比較演算子を使用して比較を行いますが、Elixirでは、値の比較において`<`,`<=`,`==`,`>=`,`>`の演算子を使用することができます。また、DateTimeやNaiveDateTimeオブジェクトを使用することもできます。詳しくは[Elixir公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#comparisons)を参照してください。

## 関連情報：

- [Elixir公式ドキュメント](https://elixir-lang.org/getting-started/basic-types.html#comparisons)
- [プログラミング言語比較：Elixir vs Ruby](https://yamaguchiyuto.hatenablog.com/entry/2019/07/02/223000)
- [日付処理に関するElixirの書き方](https://qiita.com/stanaka/items/4dadae8cc8e2456ad08c)