---
title:                "PHP: ランダムナンバーの生成"
simple_title:         "ランダムナンバーの生成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ？
ランダムな数字を生成することのメリットは何でしょうか？プログラマーの皆さんは普段、どのようにランダムな数字を利用しているのでしょうか？今回は、PHPでランダムな数字を生成する方法についてお話しします。

## 方法
PHPでは、rand()関数やmt_rand()関数を使用してランダムな数字を生成することができます。例えば、
```PHP
$random_number = rand(1, 10);
```
このように書くと、1から10までのランダムな数字を$random_numberに代入することができます。

また、mt_rand()関数では、より高度な乱数を生成することができます。例えば、
```PHP
$random_number = mt_rand(100, 1000);
```
このように書くと、100から1000までのランダムな数字を$random_numberに代入することができます。

さらに、PHPではrandom_int()関数を使用することで、よりセキュアなランダムな数字を生成することができます。例えば、
```PHP
$random_number = random_int(1, 10);
```
このように書くと、1から10までのランダムな数字を$random_numberに代入することができます。

## ディープダイブ
PHPにおけるランダムな数字の生成には、様々な方法があります。また、乱数生成にはさまざまなアルゴリズムが利用されています。例えば、メルセンヌ・ツイスターやXorshiftなどがあります。プログラマーの皆さんは、自分のプロジェクトに適した乱数生成の方法を選択することができるように、これらのアルゴリズムについても理解しておくことが重要です。

## その他
次に紹介する記事も参考にしてみてください。

- [PHP: rand - Manual](https://www.php.net/manual/en/function.rand.php)
- [PHP: mt_rand - Manual](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP: random_int - Manual](https://www.php.net/manual/en/function.random-int.php)
- [Random number generation - Wikipedia](https://en.wikipedia.org/wiki/Random_number_generation)