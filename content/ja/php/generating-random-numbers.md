---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:29.355562-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダムな数字を生成するって？コンピュータが予測不可能な数を出すことさ。なぜ使うのかって？ゲーム、セキュリティ、データ分析など、多様な場面で必要だからだよ。

## How to: (実践方法)
PHPでランダムな数字を生成するには、いくつか関数を使えばOK。`rand()`と`mt_rand()`がポピュラー。`random_int()`はセキュリティが要る場合には一番いい選択肢だよ。

```php
// シンプルなrand()の使用例
$randomNumber = rand(0, 100); // 0から100の間でランダムな値を出すよ
echo $randomNumber;

// mt_rand()はrand()より速いよ
$betterRandomNumber = mt_rand(0, 100);
echo $betterRandomNumber;

// セキュリティ重視ならrandom_int()がベスト
$secureRandomNumber = random_int(0, 100);
echo $secureRandomNumber;
```

## Deep Dive (掘り下げ)
昔は`rand()`と`mt_rand()`がよく使われていたけど、シード値に基づいた乱数生成方法は予測可能性があるんだ。だからセキュリティが大事なところでは`random_int()`を使いたい。これは暗号学的に安全な乱数を提供するからね。また、PHP 7以降では`random_bytes()`関数も使えるよ。

## See Also (関連情報)
- PHP Manual on random numbers: [php.net](https://www.php.net/manual/en/book.math.php)
- Wikipedia on Pseudorandom number generators: [wikipedia.org](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- PHP The Right Way - Random Number Generation: [phptherightway.com](http://www.phptherightway.com/#random_number_generation)
