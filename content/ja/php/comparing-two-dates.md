---
title:    "PHP: 日付の比較"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ

日付を比較することは、プログラミングにおいて重要なスキルです。例えば、ユーザーの誕生日をチェックするためや、期限が過ぎたタスクを削除するために日付を比較する必要があります。この記事では、PHPを使って2つの日付を比較する方法をご紹介します。

## 使い方

日付を比較するには、`strtotime()`関数を使います。この関数は、人間が理解しやすい形式の日付をUNIXタイムスタンプに変換します。次に、`date()`関数を使ってUNIXタイムスタンプを任意の形式の日付に変換します。

例えば、以下のようにコードを書くことで、2つの日付が等しいかどうかをチェックすることができます。

```PHP
$first_date = "2020-01-01";
$second_date = "2020-01-01";

if( strtotime($first_date) == strtotime($second_date) ){
    echo "2つの日付は同じです。";
} else {
    echo "2つの日付は異なります。";
}
```

上記のコードを実行すると、`2つの日付は同じです。`という結果が得られます。同様に、大小を比較することもできます。

```PHP
$first_date = "2020-01-01";
$second_date = "2019-12-31";

if( strtotime($first_date) > strtotime($second_date) ){
    echo "最初の日付の方が未来です。";
} else {
    echo "2つの日付は同じか、もしくは最初の日付の方が過去です。";
}
```

上記のコードを実行すると、`最初の日付の方が未来です。`という結果が得られます。

## 深堀り

PHPの日付比較には、さまざまなオプションがあります。例えば、`date_diff()`関数を使うことで、2つの日付の差を求めることができます。また、`DateTime`クラスを使うことで、より柔軟な日付操作が可能になります。

さらに、日付を扱う際には、ロケールやタイムゾーンにも注意が必要です。これらを考慮することで、より正確な日付比較が行えるようになります。

## 参考リンク

- [PHP: strtotime関数](https://www.php.net/manual/ja/function.strtotime.php)
- [PHP: date関数](https://www.php.net/manual/ja/function.date.php)
- [PHP: date_diff関数](https://www.php.net/manual/ja/function.date-diff.php)
- [PHP: DateTimeクラス](https://www.php.net/manual/ja/class.datetime.php)
- [PHP: ロケール](https://www.php.net/manual/ja/function.setlocale.php)
- [PHP: タイムゾーン](https://www.php.net/manual/ja/timezones.php)

## 関連リンク

- [PHP: 日付と時刻の操作](https://www.php.net/manual/ja/book.datetime.php)
- [PHPで日付を操作する方法](https://www.sejuku.net/blog/29224)
- [PHPで、年・月・日・曜日・イベント日数の日付データを取得しよう](https://www.linuc.org/post/86)