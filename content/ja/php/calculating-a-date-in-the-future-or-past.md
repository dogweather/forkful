---
title:                "未来または過去の日付の計算"
html_title:           "PHP: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なに&なぜ？
過去や未来の日付を計算するとは、特定の日から一定の期間を増減させた新しい日付を求めることです。プログラマーがこれを行う理由は、アプリケーションやウェブサイトで特定の日付に関連する処理を行う必要があるからです。

## 方法：
```
<?php
// 1日後の日付を取得
echo date('Y-m-d', strtotime('+1 day'));
// 出力：今日の日付の翌日

// 1週間後の日付を取得
echo date('Y-m-d', strtotime('+1 week'));
// 出力：今日の日付に1週間を足した日付

// 1ヶ月後の日付を取得
echo date('Y-m-d', strtotime('+1 month'));
// 出力：今日の日付に1ヶ月を足した日付

// 1年後の日付を取得
echo date('Y-m-d', strtotime('+1 year'));
// 出力：今日の日付に1年を足した日付
?>
```

## 詳細:
- 過去や未来の日付を計算する方法は、PHPの内部関数である`date()`と`strtotime()`を使用することで実現できます。
- date()関数は、指定したフォーマットに基づいて日付を表示します。
- strtotime()関数は、日付文字列を解析し、Unixタイムスタンプ（1970年1月1日からの秒数）を返します。
- 過去や未来の日付を計算する際、単位となる値（日数、週数、月数、年数など）を第二引数に指定する必要があります。
- 上記の例では、計算によって新しい日付が得られるため、変数に代入せずに直接echo文で出力しています。

## 関連リンク:
- [PHPのdate()関数のドキュメント](https://www.php.net/manual/en/function.date.php)
- [PHPのstrtotime()関数のドキュメント](https://www.php.net/manual/en/function.strtotime.php)