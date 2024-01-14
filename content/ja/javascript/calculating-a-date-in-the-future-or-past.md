---
title:                "Javascript: 将来または過去の日付の計算"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ 未来や過去の日付を計算するか

日付を計算する理由は様々です。例えば、テキストメッセージやメールの送信予約をしたい場合や、製品の発売日やイベントの日程を計画する際には、未来の日付を計算する必要があります。また、過去の日付を計算することで、何日前に出来事が起こったかを知ることができます。このように、日付を計算することは日常生活やビジネスにおいて重要な役割を果たしています。

## 日付を計算する方法

日付を計算する方法は様々ありますが、Javascriptを使用することで簡単に実装することができます。まずは、計算したい日付をDateオブジェクトとして宣言します。次に、未来の日付を計算する場合は、Dateオブジェクトの`setDate()`メソッドを使用し、引数に未来の日数を指定します。過去の日付を計算する場合は、同じように`setDate()`メソッドを使用しますが、引数に過去の日数のマイナス値を指定します。最後に、計算結果を`getFullYear()`、`getMonth()`、`getDate()`メソッドを使用して取得することができます。

```Javascript
// 未来の日付を計算する例
let today = new Date(); // 現在の日付を取得
today.setDate(today.getDate() + 10); // 10日後の日付を計算
console.log(today.getFullYear()); // 2021
console.log(today.getMonth() + 1); // 8
console.log(today.getDate()); // 24

// 過去の日付を計算する例
let today = new Date(); // 現在の日付を取得
today.setDate(today.getDate() - 5); // 5日間前の日付を計算
console.log(today.getFullYear()); // 2021
console.log(today.getMonth() + 1); // 7
console.log(today.getDate()); // 19
```

## 日付を計算する上での深層掘り

日付を計算する際には、異なるタイムゾーンや夏時間などの考慮が必要になることがあります。その場合、Moment.jsなどのライブラリを使用すると、より正確な計算が可能になります。また、特定の基準日から数えた経過日数を計算する場合は、`getDate()`メソッドではなく`getTime()`メソッドを使用して、数値を取得することができます。

## 参考になるリンク

- [Dateオブジェクト - MDN](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - 公式サイト](https://momentjs.com/)
- [日付計算の基礎知識 - Qiita](https://qiita.com/akey/items/5b93d33eb068eb26adfa)