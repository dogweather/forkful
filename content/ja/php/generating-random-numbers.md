---
title:                "PHP: ランダムな数値を生成する"
programming_language: "PHP"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
乱数を生成することに取り組む理由は、様々です。たとえば、ゲームや乱数を必要とするアルゴリズムを作る場合には、ランダムな値を生成する必要があります。また、アプリケーションのプライバシーやセキュリティのテストを行う際にも、乱数を使うことがあります。 

## 作り方
乱数を生成する方法はいくつかありますが、PHPには標準的な乱数生成関数が用意されています。以下のように、random_int関数を使うことで、指定された範囲内の整数値を生成することができます。

```PHP
<?php
$random_number = random_int(1, 100); 
echo $random_number;
```

この例では、1から100の範囲内でランダムな整数値を生成し、その値を出力しています。実行すると、毎回異なる値が出力されることがわかります。また、random_int関数の代わりに、mt_randやrandといった関数を使うこともできます。

## 深堀り
乱数生成は、コンピューターにおいて完全にランダムな値を生成することは不可能です。そのため、乱数生成アルゴリズムでは、擬似乱数を生成します。これは、ランダムな値を生成するための一連の計算式やルールのことです。PHPにおける乱数生成関数は、独自の乱数生成アルゴリズムを使用しています。また、アルゴリズムの妥当性やセキュリティ上の問題については、常に議論の的となっています。

## 併せて参照してほしい記事
- [PHPマニュアル：random_int](https://www.php.net/manual/ja/function.random-int.php)
- [乱数の基礎知識｜プログラマーなら知っておきたいランダム性，偶然性](https://thinkit.co.jp/article/12504)
- [乱数“エンジン”の違いによるセキュリティ上の考慮](https://www.atmarkit.co.jp/ait/articles/1410/02/news139.html)