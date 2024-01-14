---
title:                "PHP: 「日付の比較」"
programming_language: "PHP"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ二つの日付を比較するのか
日付を比較することは、プログラミングで頻繁に使われるタスクの一つです。例えば、特定の日付以降のデータを取得するときや、期限を設定するときなどに使われます。この記事では、PHPで二つの日付を比較する方法を紹介します。

## 方法
まず初めに、二つの日付を比較するためには、PHPの比較演算子を使用する必要があります。以下の例では、`>`、`<`、`==`の演算子を使用して、二つの日付の大小や等しさを比較しています。

```PHP
<?php
$date1 = date_create("2021-01-01");
$date2 = date_create("2021-05-01");

// $date1が$date2よりも前かどうかを比較する
if ($date1 < $date2){
    echo "日付1は日付2よりも前です。";
}

// $date1が$date2よりも後かどうかを比較する
elseif ($date1 > $date2){
    echo "日付1は日付2よりも後です。";
}

// $date1と$date2が等しいかどうかを比較する
else{
    echo "日付1と日付2は等しいです。";
}
?>
```

上記のコードを実行すると、以下のような出力が得られます。

```
日付1は日付2よりも前です。
```

また、日付を文字列として比較するには、`strcmp()`関数を使用することもできます。以下の例では、`strcmp()`関数を使用して二つの日付を比較しています。

```PHP
<?php
$date1 = "2021-01-01";
$date2 = "2021-05-01";

// $date1が$date2よりも前かどうかを比較する
if (strcmp($date1, $date2) < 0){
    echo "日付1は日付2よりも前です。";
}

// $date1が$date2よりも後かどうかを比較する
elseif (strcmp($date1, $date2) > 0){
    echo "日付1は日付2よりも後です。";
}

// $date1と$date2が等しいかどうかを比較する
else{
    echo "日付1と日付2は等しいです。";
}
?>
```

同様に、上記のコードを実行すると以下のような出力が得られます。

```
日付1は日付2よりも前です。
```

## ディープダイブ
二つの日付を比較する際には、日付のフォーマットが一致していることが重要です。例えば、`Y-m-d`のフォーマットで日付を定義した場合、比較する日付も同じフォーマットで定義する必要があります。また、PHPの`date()`関数を使用することで、日付のフォーマットを任意の形式に変更することもできます。詳しくはPHPの公式ドキュメントを参照してください。

## See Also
- [PHPマニュアル - 日付と時刻の処理](https://www.php.net/manual/ja/book.datetime.php)
- [PHPマニュアル - 比較演算子](https://www.php.net/manual/ja/language.operators.comparison.php)
- [PHPマニュアル - strcmp関数](https://www.php.net/manual/ja/function.strcmp.php)