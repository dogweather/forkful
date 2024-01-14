---
title:    "PHP: 将来または過去の日付の計算"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

「なぜ日付の計算をするのか？」

日付の計算をする理由はさまざまです。例えば、特定のイベントや記念日を予定する場合や、ある日から何日後にある日付を知りたい場合などが挙げられます。PHPを使えば、未来や過去の日付を簡単に計算することができます。

「方法」

まずは、``date``関数を使って現在の時刻を取得します。次に、``strtotime``関数を使って未来や過去の日付を計算します。``strtotime``関数は、引数として日付のフォーマットや時間の単位を指定することができます。以下は、ある日から5日後の日付を取得する例です。

```PHP
$date = date("Y-m-d"); // 現在の日付を取得
$new_date = strtotime("+5 days", strtotime($date)); // 現在の日付に5日を足して新しい日付を取得
echo date("Y-m-d", $new_date); // 新しい日付をフォーマットして出力
```

もし、ある日から1週間後の日付を取得したい場合は、下のようにコードを変更します。

```PHP
$new_date = strtotime("+1 week", strtotime($date));
```

同様に、過去の日付を計算することもできます。例えば、ある日から1か月前の日付を取得する場合は、以下のようにコードを書きます。

```PHP
$new_date = strtotime("-1 month", strtotime($date));
```

「深堀り」

PHPでは、``strtotime``関数の引数として有効な日付のフォーマットや時間の単位が多数あります。例えば、``next week``や``+3 months``などの指定方法もあります。また、``strtotime``関数は、UNIXタイムスタンプを作成するための方便でもあります。UNIXタイムスタンプとは、1970年1月1日からの経過秒数のことで、主に日付や時間の計算に用いられます。

「参考リンク」

- [date関数の使い方 | PHPマニュアル](https://www.php.net/manual/ja/function.date.php)
- [strtotime関数の使い方 | PHPマニュアル](https://www.php.net/manual/ja/function.strtotime.php)
- [UNIXタイムスタンプとは | GeeksforGeeks](https://www.geeksforgeeks.org/javascript-date-object/)