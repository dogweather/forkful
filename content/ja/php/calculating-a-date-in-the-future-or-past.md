---
title:                "未来や過去の日付を計算する"
html_title:           "PHP: 未来や過去の日付を計算する"
simple_title:         "未来や過去の日付を計算する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を将来や過去に計算することが必要とされる理由はさまざまです。例えば、イベントの日程を決める際や、定期的に発生するタスクの期限を追跡する際には、日付の計算が必要になるでしょう。PHPを使えば簡単に日付の計算ができるので、ぜひ覚えておきましょう。

## 方法

PHPで日付を計算するには、DateクラスやDateTimeクラスを使います。例えば、以下のコードでは、明日の日付を計算して出力する方法が分かります。

```PHP
//今日の日付を取得
$today = new DateTime();

//明日の日付を計算
$tomorrow = $today->modify('+1 day');

//出力
echo $tomorrow->format('Y-m-d');
```

このコードを実行すると、現在の日付に1日足された日付が出力されます。

## 深堀り

日付を計算する際には、時差や夏時間の影響を考慮する必要があります。また、指定した日付が週末や祝日かどうかを判定するメソッドも用意されています。PHPの公式ドキュメントやオンラインのサンプルコードを参考に、さまざまな方法で日付を計算することができます。

## See Also

- [PHP: Dateクラスの使用方法](https://www.php.net/manual/ja/class.datetime.php)
- [PHP: DateTimeクラスの使用方法](https://www.php.net/manual/ja/class.datetime.php)