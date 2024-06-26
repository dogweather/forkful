---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:21.812667-07:00
description: ''
lastmod: '2024-04-05T22:38:42.273567-06:00'
model: gpt-4-0125-preview
summary: "C\u8A00\u8A9E\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  `<stdlib.h>`\u306E\u4E00\u90E8\u3067\u3042\u308B`rand()`\u95A2\u6570\u3092\u4F7F\
  \u7528\u3057\u3066\u4E71\u6570\u3092\u751F\u6210\u3067\u304D\u307E\u3059\u3002\u30C7\
  \u30D5\u30A9\u30EB\u30C8\u3067\u306F\u3001`rand()`\u306F0\u304B\u3089`RAND_MAX`\uFF08\
  `<stdlib.h>`\u3067\u5B9A\u7FA9\u3055\u308C\u305F\u5B9A\u6570\uFF09\u306E\u7BC4\u56F2\
  \u306E\u7591\u4F3C\u4E71\u6570\u3092\u751F\u6210\u3057\u307E\u3059\u3002\u7BC4\u56F2\
  \u3092\u3088\u308A\u7D30\u304B\u304F\u5236\u5FA1\u3059\u308B\u305F\u3081\u306B\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F`rand()`\u306E\u51FA\u529B\u3092\u64CD\
  \u4F5C\u3067\u304D\u307E\u3059\u3002"
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法:
C言語では、標準ライブラリ`<stdlib.h>`の一部である`rand()`関数を使用して乱数を生成できます。デフォルトでは、`rand()`は0から`RAND_MAX`（`<stdlib.h>`で定義された定数）の範囲の疑似乱数を生成します。範囲をより細かく制御するために、プログラマーは`rand()`の出力を操作できます。

ここに0から99の間の乱数を生成する簡単な例を示します:

```c
#include <stdio.h>
#include <stdlib.h> // rand()とsrand()用
#include <time.h>   // time()用

int main() {
    // 乱数生成器にシードを提供
    srand((unsigned) time(NULL));

    // 0から99の間の乱数を生成
    int randomNumber = rand() % 100;

    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

このプログラムを実行するたびに、出力例は異なる可能性があります:

```
Random Number: 42
```
異なる範囲内で乱数を生成するために、モジュラス演算子(`%`)を適宜調整できます。たとえば、`rand() % 10`は0から9までの数を生成します。

疑似乱数生成器を現在時刻(`time(NULL)`)でシード化する(`srand()`呼び出し)ことは、プログラムの実行ごとに異なる乱数のシーケンスを保証することを意味していることが重要です。シード化(`srand()`)せずに、`rand()`を使うと、プログラムを実行するたびに同じ数値のシーケンスを生成します。

## 深い分析
`rand()`関数とそのシード化の対応`srand()`は数十年に渡ってC標準ライブラリの一部です。これらは、ランダムであるように見える数値のシーケンスを生成するアルゴリズムに基づいています。これが「疑似ランダム」という用語の由来です。`rand()`内の基本アルゴリズムは通常、線形合同生成器（LCG）です。

`rand()`と`srand()`は多くのアプリケーションにとって十分ですが、ランダム性の質や予測可能性に関して既知の制限があります。暗号化操作など、高品質のランダム性を要求するアプリケーションの場合、`/dev/random`や`/dev/urandom`（Unix系システム上）、または暗号ライブラリが提供するAPIなどの代替手段が検討されるべきです。

C11の導入により、ISO C標準は同時操作のより洗練された制御を提供する新しいヘッダー`<stdatomic.h>`を含みましたが、これはランダム性に直接関連するものではありません。C言語で真のランダム性を求める場合、開発者はしばしば、より良いアルゴリズムを提供したり、ハードウェアのエントロピーソースを利用したりするプラットフォーム固有または外部のライブラリに目を向けます。

`rand()`は疑似乱数を生成するためのシンプルでアクセスしやすい手段として機能しますが、その出力の質と予測可能性により、現代のアプリケーションでの使用は限定されています。特にセキュリティ意識の高いアプリケーションにおいて、より堅牢なソリューションが求められる場合、標準ライブラリを超えて探求することが強く推奨されます。
