---
title:    "TypeScript: ランダム数字を生成する"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 説明
乱数生成は、プログラミングにとって重要な機能の一つです。乱数生成により、ランダムなデータを生成することができ、プログラムの予測可能性を下げることができます。乱数生成を使うことで、さまざまなシミュレーションやゲーム、暗号化など、さまざまな用途に利用することができます。

## 使い方
TypeScriptでは、Mathオブジェクトのrandomメソッドを使うことで、簡単に乱数を生成することができます。以下のコードを参考にしてください。

```TypeScript
// 0から10までの範囲の整数を生成する例
let randomNum = Math.floor(Math.random() * 11);
console.log(randomNum); // 0から10までのランダムな整数が出力される


// 1から100までの範囲の整数を生成する例
let randomInt = Math.floor(Math.random() * 100) + 1;
console.log(randomInt); // 1から100までのランダムな整数が出力される


// 0から1までの範囲の小数を生成する例
let randomDec = Math.random();
console.log(randomDec); // 0から1までのランダムな小数が出力される
```

乱数を生成する際には、Math.random()メソッドの値を適切に加工することで、必要な範囲や型の乱数を生成することができます。

## 深堀り
乱数生成には様々なアルゴリズムがあり、それぞれに特徴があります。しかし、重要なのはランダム性を保証することです。プログラムで使われる乱数は、本当にランダムな値かどうかを検証することが重要です。そのためには、擬似乱数生成器として知られるアルゴリズムが使われています。これは、ランダムな値のように見せかける計算式ですが、実際には事前に設定されたシード値をもとに計算を行い、乱数のように見える値を返します。

しかし、擬似乱数生成器で生成した値は、特定の条件下では予測可能な値になってしまう可能性があります。そのため、セキュリティや暗号処理など、ランダム性が重要な場面では、より高度なアルゴリズムを使用する必要があります。

## 参考情報
- [MDN - Math.random()](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [乱数を扱う方法 - TypeScript Deep Dive 日本語版](https://typescript-jp.gitbook.io/deep-dive/main-1/type-checking/random-how-to)
- [JavaScript中の乱数生成器の消耗品性と定型 - こだわりの技術ブログ](https://micro-tech.tokyo/2018/10/16/72/)
- [乱数生成について - Think IT](https://thinkit.co.jp/article/18904)

## 関連リンク
- [Blogに説明を加えたリポジトリ - GitHub](https://github.com/username/blog)
- [JavaScript Mathオブジェクトの参考ドキュメント - Mozilla Developer Network](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Math)