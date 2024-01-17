---
title:                "「日付の比較」"
html_title:           "PHP: 「日付の比較」"
simple_title:         "「日付の比較」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何を比較するのか？
デートを比較するとは、2つの日付を比べることです。プログラマーがデートを比較するのは、さまざまな目的があります。例えば、2つのイベントの間隔を計算する、ユーザーがサイトにアクセスしてから経過した時間を計算するなどがあります。

## 方法：
```PHP
$first_date = date_create('2021-01-01');
$second_date = date_create('2021-01-05');

// 日付の比較
if ($first_date < $second_date) {
  echo "さようなら、離れて暮らすことになった日の数は、他の人たちより少ない。";
}
```

## 詳細解説
デートの比較は、過去から現在までの時間を計算するために使用されてきた古い手法です。しかし、PHPでは便利なビルトイン関数が用意されています。例えば、`date_diff()`関数を使用することで、簡単に日付の間隔を計算することができます。また、`DateTime`クラスを使用することで、より複雑な計算も可能になります。

代替手段としては、UNIXタイムスタンプの使用があります。UNIXタイムスタンプは、1970年1月1日からの秒数を表す数値です。これを使用することで、日付の比較をより高速かつ精確に行うことができます。

## 関連リンク
- [`date_diff()` 関数](https://www.php.net/manual/ja/function.date-diff.php)
- [DateTime クラス ](https://www.php.net/manual/ja/class.datetime.php)