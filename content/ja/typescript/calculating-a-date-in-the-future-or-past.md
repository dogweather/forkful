---
title:                "未来または過去の日付の計算"
html_title:           "TypeScript: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何かとは？
日付の計算を一定のルールに従って行うことを指します。プログラマーは、特定の日付の未来や過去を計算する必要があるため、このような計算を行います。

## 方法：
```TypeScript
// 現在から10日後の日付を計算する例
const currentDate = new Date();
const futureDate = new Date(currentDate.setDate(currentDate.getDate() + 10));
console.log(futureDate.toDateString()); // フォーマットされた日付（例：Mon Aug 16 2021）
```
```TypeScript
// 指定された日付から5年前の日付を計算する例
const pastDate = new Date('2015-08-16');
const fiveYearsAgo = new Date(pastDate.setFullYear(pastDate.getFullYear() - 5));
console.log(fiveYearsAgo.toDateString()); // フォーマットされた日付（例：Thu Aug 16 2015）
```

## 詳細：
日付の計算は、歴史的な背景や選択肢、実装の詳細について深く調べる価値があります。過去の日付の計算は、歴史的な出来事や記念日を特定するのに役立ちます。一方、未来の日付の計算は、予定や期限を設定するのに役立ちます。日付の足し算や引き算をする方法はいくつかありますが、JavaScriptやTypeScriptでは、Dateオブジェクトのメソッドを使って計算を行うことができます。

## 関連情報：
- [Dateオブジェクトのドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScriptにおける日付の扱い方](https://www.sejuku.net/blog/77361#i-2)