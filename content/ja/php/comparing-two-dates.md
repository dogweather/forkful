---
title:    "PHP: 2つの日付の比較"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
PHPプログラミングブログを読んでくださりありがとうございます！今日は、日本語読者の皆さんにとって役立つコンテンツをお届けします。今回は、日付を比較するプログラムについてお話しします。日付比較をすることで、より正確な日付変換やデータ処理を行うことができます。是非ご覧ください！

## 方法
日付比較をするには、まず2つの日付をプログラム内で読み込む必要があります。これには、 `strtotime()` 関数を使用します。例えば、以下のように日付を読み込むことができます。

```PHP
$date1 = strtotime("2021-01-01");
$date2 = strtotime("2021-05-05");
```

次に、 `ts1` と `ts2` の2つのタイムスタンプを比較します。比較には `if` 文を使用し、条件を設定します。例えば、以下のように比較することができます。

```PHP
if($date1 < $date2) {
    echo "日付1は日付2よりも前です";
} elseif($date1 > $date2) {
    echo "日付1は日付2よりも後です";
} else {
    echo "日付1と日付2は同じです";
}
```

もし、日付を文字列から読み込む場合は、 `strtotime()` 関数を使用する代わりに `DateTime` オブジェクトを使用することもできます。例えば、以下のように使用することができます。

```PHP
$date1 = new DateTime("2021-01-01");
$date2 = new DateTime("2021-05-05");

if($date1 < $date2) {
    echo "日付1は日付2よりも前です";
} elseif($date1 > $date2) {
    echo "日付1は日付2よりも後です";
} else {
    echo "日付1と日付2は同じです";
}
```

以上のように、日付比較はとても簡単に行うことができます。ぜひ、実際にコードを書いてみて、日付比較を試してみてください！

## 深堀り
日付比較についてもっと知りたい方は、 `strtotime()` 関数や `DateTime` オブジェクトについて詳しく学ぶことをお勧めします。また、PHPの公式ドキュメントや関連するブログ記事も参考になります。日付比較を使った実際のプログラミング例や問題解決のヒントを見つけることもできます。

## 参考リンク
- [PHP公式ドキュメント](https://www.php.net/manual/ja/function.strtotime.php)
- [PHP Date and Time functionsトピック](https://www.w3schools.com/php/php_ref_date.asp)
- [PHPで日付比較をする方法](https://qiita.com/nayopu/items/908159b5e79065873733)