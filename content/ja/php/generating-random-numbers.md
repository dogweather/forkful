---
title:    "PHP: 「ランダムな数字の生成」"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## なぜ？

プログラミングで乱数を生成するのは、重要なスキルの一つです。乱数は、ゲーム開発やランダムな選択を行うアプリケーションの中で使用されます。また、セキュリティの観点からも重要であり、暗号化にも使用されます。

## 作り方

```PHP
// ランダムな整数を生成する
$random_int = rand();

echo $random_int; // 例：422613

// 特定の範囲内のランダムな整数を生成する
$random_int = rand(1, 10); // 1から10の間の整数を生成する
echo $random_int; // 例：7
```

```PHP
// ランダムな小数を生成する
$random_float = lcg_value();

echo $random_float; // 例：0.91392444600409

// 特定の範囲内のランダムな小数を生成する
$random_float = mt_rand() / mt_getrandmax(); // 0から1の間の小数を生成する
echo $random_float; // 例：0.72491865956541
```

ランダムな結果を得るために、様々な関数があります。適した関数を選択し、引数を設定することで、自分のニーズに合った乱数を生成することができます。

## 深堀り

乱数生成には、擬似乱数生成と真の乱数生成の2種類があります。擬似乱数生成では、アルゴリズムによって予測不能な乱数を生成します。一方、真の乱数生成では、外部からの要因によって乱数を生成します。

PHPでは、擬似乱数生成が使用されます。そのため、セキュリティの観点からは注意が必要です。また、mt_rand()やlcg_value()のようなビルトイン関数の性能は、使用する環境によって大幅に異なる可能性があります。そのため、乱数を高速に生成したい場合は、rand()を使用することを推奨します。

## 参考

- [PHP: mt_rand - Manual](https://www.php.net/manual/ja/function.mt-rand.php)
- [PHP: lcg_value - Manual](https://www.php.net/manual/ja/function.lcg-value.php)
- [A Beginner's Guide to Random Number Generation in PHP](https://www.sitepoint.com/beginners-guide-to-random-number-generation-in-php/)