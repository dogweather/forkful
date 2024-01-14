---
title:                "TypeScript: 「未来または過去の日付の計算」"
simple_title:         "「未来または過去の日付の計算」"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

未来や過去の日付を計算する必要があるか、その理由を簡単に説明します。

## 方法

日付を計算するためのサンプルコードと出力の例を以下のように```TypeScript ... ```コードブロックで示します。

```TypeScript
// 今日の日付を取得
const today = new Date();

// 10日後の日付を計算
const dateInFuture = new Date(today.getTime() + (10 * 24 * 60 * 60 * 1000));

console.log(dateInFuture);
// 出力結果: 2021-XX-XXTXX:XX:XX.XXXZ (今日の日付から10日後の日付となる)

// 1年前の日付を計算
const dateInPast = new Date(today.getFullYear() - 1, today.getMonth(), today.getDate());

console.log(dateInPast);
// 出力結果: 2020-XX-XXTXX:XX:XX.XXXZ (1年前の日付となる)
```

## 深堀り

未来や過去の日付を計算する際には、現在の日付を取得し、その日付を基準にして計算を行います。また、JavaScriptのDateオブジェクトを使用することで、さまざまな計算が可能です。例えば、今日の日付から10日後や1年前の日付を計算することができます。

## See Also

以下のリンクを参考に、未来や過去の日付を計算する方法について学びましょう。

- [MDN web docs - Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript公式ドキュメント - Dateオブジェクト](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html#date-types)