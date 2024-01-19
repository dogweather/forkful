---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何と何のため？ (What & Why?)

日付を比較することは、二つの異なる時間ポイントを比較するためのプロセスです。これは、期限管理、プロジェクトスケジューリング、または時間ベースのアクティビティーのロギングなどの用途があります。

## 使い方 (How to:)

あなたはPHPの組み込み関数`strtotime()`を使って二つの日付を比較することができます。以下に例を示します：

```PHP
$deadline = strtotime("2022-12-30");
$today = strtotime("today");

if ($deadline > $today) {
    echo "まだ期限内です！";
} else {
    echo "締切を過ぎてしまいました...。";
}
```
このプログラムは、締切が今日の日付よりも未来であれば "まだ期限内です！" を出力し、そうでなければ "締切を過ぎてしまいました...。" を出力します。

## 深堀り (Deep Dive)

日付の比較は実は決して新しいアイデアではありません、しかしその具体的な実装方法はプログラミング言語や目的によって異なります。たとえば、`strtotime()` の代わりに DateTime オブジェクトを使う方法もあります。

```PHP
$date1 = new DateTime("2022-12-30");
$date2 = new DateTime("today");

if ($date1 > $date2) {
    echo "まだ期限内です！";
} else {
    echo "締切を過ぎてしまいました...。";
}
```
以上のコードは、同じ結果を生成しますが、DateTime オブジェクトはもっと多くの機能と設定を持っています。たとえば、時間帯のサポートなどがあります。

## 参照信息 (See Also)

以下のリンクで次のトピックについてもっと詳しく学ぶことができます:

1. [strtotime() PHP Official Documentation](https://www.php.net/manual/function.strtotime)
2. [DateTime Objects PHP Documentation](https://www.php.net/manual/class.datetime)
3. [PHP: Comparing Dates context in StackOverflow](https://stackoverflow.com/questions/2532729/daylight-saving-time-and-timezone-best-practices)
4. [PHP Date/Time Functions Tutorial](https://www.tutorialspoint.com/php/php_date_time_functions)