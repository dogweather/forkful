---
title:                "ランダムな数の生成"
html_title:           "PHP: ランダムな数の生成"
simple_title:         "ランダムな数の生成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何ができるの？
ランダムな数字を生成するとは、プログラマーがプログラミング言語を使用して、コンピューターを確率的に操作することです。プログラマーは、ランダムなデータを必要とするさまざまなケースでランダムな数字を生成する必要があります。

## 方法：
```PHP

// 1から10までのランダムな数字を生成する
echo mt_rand(1,10);

// 数字の配列からランダムな要素を選択する
$numbers = [1, 2, 3, 4, 5];
echo array_rand($numbers);
```

実行結果:

```
8  // mt_rand()の場合、1から10までの間のランダムな数字が出力される
3  // array_rand()の場合、1から5までのインデックスからランダムに選ばれる
```

## 詳細情報：
ランダムな数字の生成は、コンピューターにおいて重要な機能です。以前は、ランダムな数字を生成するために線形合同法と呼ばれるアルゴリズムが使用されていましたが、現在ではメルセンヌ・ツイスターと呼ばれるアルゴリズムが主流です。PHPでは、擬似乱数を生成するための多くの関数が用意されており、開発者は必要に応じて使い分けることができます。また、ランダムな数字の生成には、外部のAPIやハードウェアデバイスを使用する方法もあります。

## 関連リンク：
- [PHP公式ドキュメンテーション - ランダムな整数の生成](https://www.php.net/manual/en/function.mt-rand.php)
- [PHP公式ドキュメンテーション - ランダムな要素の選択](https://www.php.net/manual/en/function.array-rand.php)
- [メルセンヌ・ツイスター - Wikipedia](https://ja.wikipedia.org/wiki/%E3%83%A1%E3%83%AB%E3%82%BB%E3%83%B3%E3%83%8C%E3%83%BB%E3%83%84%E3%82%A4%E3%82%B9%E3%82%BF)
- [RAND and MT_RAND: What They Are and How They Differ - W3Schools](https://www.w3schools.com/php/func_math_rand.asp)