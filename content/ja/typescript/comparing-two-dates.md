---
title:    "TypeScript: 「日付を比較する」"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ日付を比較するのか

プログラミングにおいて、日付を比較することは非常に重要です。例えば、あなたのアプリケーションにおいて、特定の日付が過去なのか未来なのかを判断する必要があります。そのような場合、日付の比較が必要になります。また、日付をソートする際にも日付の比較が使用されます。日付を比較することで、プログラムをより正確で効率的に動作させることができます。

## 方法

TypeScriptを使用して日付を比較する方法を紹介します。まず、比較したい2つの日付をDateオブジェクトとして定義します。次に、比較演算子を使用して、2つの日付を比較します。

```
// 日付の定義
let firstDate: Date = new Date(2021, 5, 1);
let secondDate: Date = new Date(2021, 5, 15);
// 比較
console.log(firstDate > secondDate); // false
console.log(firstDate < secondDate); // true
console.log(firstDate == secondDate); // false
```

上記のコードでは、firstDateがsecondDateよりも未来の日付なので`>`演算子は`false`を返します。また、`<`演算子を使用することでfirstDateがsecondDateよりも過去の日付かどうかを判定することができます。さらに、`==`演算子を使用することで日付が完全に一致するかどうかを判定することができます。

## ディープダイブ

実際には、日付を比較する際には単純な演算子だけではなく、より複雑な条件を使用することもあります。そのような場合、Dateオブジェクトのメソッドを使用することができます。例えば、`getDate()`メソッドを使用することで日付の数字を取得することができます。また、`getFullYear()`メソッドを使用することで年を取得することができます。

```
// 日付の定義
let today: Date = new Date();
let checkDate: Date = new Date(2021, 5, 5);
// 比較
console.log(today.getDate() == checkDate.getDate()); // true
console.log(today.getFullYear() == checkDate.getFullYear()); // true
```

日付を比較する際には、以上のようなメソッドを組み合わせて使用することで、より複雑な条件を含む比較を行うことができます。

## 参考リンク

- [TypeScript 公式サイト](https://www.typescriptlang.org/)
- [Date クラス (MDN Web Docs)](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript における演算子の使用方法 (TechAcademy)](https://techacademy.jp/magazine/18736)
- [日付の比較方法について (Qiita)](https://qiita.com/@fukunaga-takumi/items/f0595c221b85ae2d7bc8)