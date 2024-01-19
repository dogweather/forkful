---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なに？なぜ？

ランダムな数値生成とは、プログラムが予測不能の数値を作り出すプロセスを指します。開発者はセキュリティー、ゲーム、シミュレーション、テストデータなどのためにこの機能を使います。

## 使い方:

ランダムな数値を生成するための一般的なPHPコードを以下に示します。

```PHP
<?php
  $randomNumber = rand(1, 50);
  echo $randomNumber;
?>
```

上記のコードは、1から50までのランダムな数値を生成し、表示します。

## 深掘り:

PHPでのランダム数値生成には、主に`rand()`関数と`mt_rand()`関数の2つが使われます。それらの違いは、`mt_rand()`が`rand()`よりも統計学的に均一な分布を持つとされる点です。

`rand()`関数は古い関数で、C言語の`rand`関数から進化したものです。一方、`mt_rand()`関数はより新しく、メルセンヌ・ツイスターと呼ばれる乱数生成アルゴリズムに基づいています。

また、より強力なセキュリティが求められる場合、もしくは暗号学的に安全なランダムな数値が必要な場合は、`random_int()`を使用するのが最適です。

## 関連情報:

- PHPの公式ドキュメンテーション: [rand()関数](https://www.php.net/manual/ja/function.rand.php)、[mt_rand()関数](https://www.php.net/manual/ja/function.mt-rand.php)、[random_int()関数](https://www.php.net/manual/ja/function.random-int.php)
- rand()とmt_rand()の[比較](https://www.php.net/manual/ja/function.mt-rand.php#refsect1-function.mt-rand-notes)
- メルセンヌ・ツイスターについての[詳細](https://ja.wikipedia.org/wiki/%E3%83%A1%E3%83%AB%E3%82%BB%E3%83%B3%E3%83%8C%E3%83%84%E3%82%A4%E3%82%B9%E3%82%BF)