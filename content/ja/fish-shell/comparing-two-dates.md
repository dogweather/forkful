---
title:                "Fish Shell: 「二つの日付を比較する」"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜこの記事を読むべきか

日付を比較することは、プログラマーにとって非常に重要です。この記事では、Fish Shellを使用して、簡単な方法で日付を比較する方法を紹介します。

## 方法

Fish Shellの```fish_date```コマンドを使用して、2つの日付を比較することができます。まず、比較したい日付のフォーマットを設定します。

```
set date1 "2020-02-01"
set date2 "2021-03-15"
```

次に、```fish_date```を使用して日付を比較します。

```
if test (fish_date <$date1 >$date2)
echo"$date1 is earlier than $date2"
end
```

上記のコードでは、```<$```と```>$```を使用して、日付を比較し、結果を出力します。```<$```は左側の値が右側の値よりも小さい場合、```>$```は右側の値が左側の値よりも大きい場合に真となります。

## 詳細

Fish Shellの```fish_date```コマンドは、GNU dateコマンドと同等の機能を持っています。したがって、様々なフォーマットで日付を比較することができます。

また、日付の比較に使用する演算子として、```<```、```<=```、```>```、```>=```、```==```、```!=```があります。これらの演算子は、日付を数値として比較することもできます。

## それでは

今回紹介した方法以外にも、Fish Shellで日付を比較する方法はいくつかあります。以下のリンクを参考に、さまざまな方法を試してみてください。

- Fish Shell公式ドキュメント：https://fishshell.com/docs/current/index.html#selecting-dates
- GNU dateコマンド公式ドキュメント：https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html

## 関連記事

- Fish Shellで変数を使って日付を操作する方法：https://example.blog/fishshell-dates-variables
- 日付のフォーマットを変更する方法：https://example.blog/how-to-change-date-format-using-fishshell