---
title:                "PHP: 「未来や過去の日付を計算する」"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ日付を未来や過去で計算するのか

日付を未来や過去で計算するのには様々な理由があります。例えば、イベントの予定日を計算するためや、期限を設定するために使用されることがあります。また、日付に関するデータを処理するプログラムを作成する際にも、日付の計算は必須の要素となります。

## 方法

未来や過去の日付を計算する方法は、PHPの「strtotime」関数を使用することで簡単に実装することができます。以下に具体的なコード例を示します。

```
<?php
// 今日の日付を取得する
$today = date('Y-m-d');

// 1週間後の日付を計算する
$future_date = date('Y-m-d', strtotime('+1 week'));

// 1ヶ月前の日付を計算する
$past_date = date('Y-m-d', strtotime('-1 month'));

// 結果を出力する
echo '今日の日付: ' . $today . PHP_EOL;
echo '1週間後の日付: ' . $future_date . PHP_EOL;
echo '1ヶ月前の日付: ' . $past_date . PHP_EOL;
```

このコードを実行すると、以下のような結果が得られます。

```
今日の日付: 2021-07-01
1週間後の日付: 2021-07-08
1ヶ月前の日付: 2021-06-01
```

## ディープダイブ

日付を未来や過去で計算する方法は、「strtotime」関数以外にも様々な方法があります。例えば、PHPの「DateTime」クラスを使用する方法や、外部のライブラリを導入する方法もあります。また、タイムゾーンの設定や特殊な日付フォーマットへの対応方法など、さまざまなテクニックも存在します。

しかし、いずれの方法を選択するにせよ、日付を計算する際には日付のフォーマットについても注意する必要があります。日付のフォーマットが間違っていると、意図しない結果が得られる可能性があります。そのため、事前に十分なテストを行うことが重要です。

# 参考リンク

[PHP公式ドキュメント - strtotime関数](https://www.php.net/manual/ja/function.strtotime.php)

[PHP公式ドキュメント - DateTimeクラス](https://www.php.net/manual/ja/class.datetime.php)

[日付と時刻の処理に便利なPHPライブラリ6選](https://qiita.com/yyoshiki41/items/3dde463064f871ecd4b1)