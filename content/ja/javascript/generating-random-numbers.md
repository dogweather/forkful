---
title:                "乱数の生成"
html_title:           "Javascript: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ
ランダムな数値を生成することによる利点を最大2文で説明します。

## 作り方
```Javascript
// 0から1までのランダムな数値を生成する関数
function createRandomNumber() {
  return Math.random();
}

// 関数を呼び出し、結果をコンソールに表示する
console.log(createRandomNumber()); // 0から1までのランダムな数値が表示される
```

## 詳細を知る
ランダムな数値を生成することによる多くの利点があります。例えば、ゲームやロト番号を決める際に使用することができます。Javascriptの場合、Mathオブジェクト内のrandomメソッドを使用することで簡単にランダムな数値を生成することができます。しかし、生成される数値には規則性があり、厳密なランダム性を求める場合は専門的なアルゴリズムが必要になります。

## See Also
- [Math.random()のドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [ランダムな数値を生成するアルゴリズムについての解説](https://www.geeksforgeeks.org/generating-random-number-in-java/)