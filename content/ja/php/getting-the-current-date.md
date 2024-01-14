---
title:                "PHP: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
現在の日付を取得する方法について学ぶ理由は多くあります。例えば、タイムスタンプを使用してWebサイト上でコンテンツを更新する必要がある場合や、定期的なデータバックアップを作成する必要がある場合などです。

## 方法
PHPを使用して現在の日付を取得するには、```date()```関数を使用します。以下の例では、日付のサイズを設定し、英語の日付フォーマットで出力しています。

```PHP
date_default_timezone_set('Asia/Tokyo');
echo date('l, F d, Y');
```

このコードを実行すると、以下のような出力が得られます。

```
Monday, October 04, 2021
```

また、PHPの```time()```関数を使用することでタイムスタンプを取得することもできます。以下の例では、Unixエポックからの秒数を取得しています。

```PHP
$timestamp = time();
```

時間の差や加算、減算などの操作が必要な場合は、PHPの```strtotime()```関数を使用することができます。この関数を使用するには、指定した日付文字列を日付として解析できる形式で入力する必要があります。例えば、1日後の日付を取得するには、以下のように記述します。

```PHP
$next_day = date('Y-m-d', strtotime('+1 day'));
echo $next_day;
```

この場合、出力は以下のようになります。

```
2021-10-05
```

## 深堀り
PHPの日付関数は、強力で柔軟性のあるものであり、様々な操作やフォーマットに対応できます。例えば、特定の日付で条件分岐するためには、```date('w')```を使用することができます。これは、今日が何曜日かを0（日曜日）から6（土曜日）の数字で返します。

また、曜日の代わりに月を条件に設定したい場合は、```date('n')```を使用することができます。これは、今日が何月かを数字で返します。

さらに、日付のフォーマットもカスタマイズすることができます。詳細な情報は、[公式ドキュメント](https://www.php.net/manual/en/function.date.php)を参照してください。

## 参考リンク
- [PHP公式ドキュメント - date()関数](https://www.php.net/manual/en/function.date.php)
- [PHP公式ドキュメント - time()関数](https://www.php.net/manual/en/function.time.php)
- [PHP公式ドキュメント - strtotime()関数](https://www.php.net/manual/en/function.strtotime.php)