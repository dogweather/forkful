---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ランダムな数値の生成 - TypeScriptでレッツトライ！

## 何？なぜ？

ランダムの数を生成するとは、予想不能な数を計算機が出力することです。プログラマがこれを行う理由は、サンプルデータの生成、セキュリティーシステム、ゲームの要素など、多岐にわたります。

## やり方：

以下はTypeScriptでどのようにランダムな数を生成するか示す例です：

```TypeScript
function getRandomNumber(min:number, max:number):number {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

console.log(getRandomNumber(1, 100));
```
上記のプログラムを実行すると、1から100までのランダムな数が出力されます。

## ディープダイブ：

ランダム数値の生成は古い問題で、乱数生成器の歴史は古代ローマのサイコロから始まりました。コンピュータが乱数を生成するためには複雑なアルゴリズムを使う必要があります。

代替手段として、ハードウェアベースのランダム数ジェネレーターや、擬似乱数ジェネレーターなどがあります。「Math.random()」関数自体はC言語の「rand()」関数から派生したもので、現実的には予測不可能な結果を提供します。

ただし、「Math.random()」が生成する数値は完全にランダムではなく、擬似ランダムな数値を生成します。完全にランダムな数値を得るには、ハードウェアレベルのランダムな発生源（例えば、放射性崩壊など）を利用する方法もありますが、これは一般的なアプリケーションではほとんど使用されません。

## 参考資料：

以下のリンクから、ランダム数の生成やその他の関連するトピックについて詳しく学べます：

- [TypeScript公式ドキュメンテーション](https://www.typescriptlang.org/docs/handbook/utility-types.html)
- [Math.random()関数を使った乱数生成](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [JavaScriptにおける乱数生成の詳細](https://v8.dev/blog/math-random)
- [乱数生成器の歴史](https://www.random.org/history/)