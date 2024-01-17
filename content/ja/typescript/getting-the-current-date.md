---
title:                "現在の日付を取得する"
html_title:           "TypeScript: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

「## 今日の日付を取得する方法とその意義？」
プログラマーが現在の日付を取得する方法について2〜3文で説明し、なぜそれを行うのかを解説します。

「## 方法：」
```TypeScript
// 現在の日付を取得する方法
const currentDate = new Date();
console.log(currentDate.toDateString()); // 例：Sat Feb 05 2022

// 今日の日付を特定の形式で取得する方法
const currentDate = new Date();
const options = { weekday: 'long', year: 'numeric', month: 'long', day: 'numeric' };
console.log(currentDate.toLocaleDateString('ja-JP', options)); // 例：2022年2月5日土曜日
```

「## 詳しい説明：」
現在の日付を取得する方法は、プログラム内で日付情報を扱う際に重要な役割を果たします。例えば、特定のイベントの日付をチェックするために使用したり、ファイルの作成日時を取得するために使用したりすることができます。代替手段として、外部のAPIやライブラリを使用することもできますが、基本的にはプログラム内で直接取得することが推奨されます。実装の詳細は、JavaScriptの標準機能であるDateオブジェクトを使用して行われます。

「## 関連情報：」
- [MDN - Dateオブジェクトのリファレンス](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScriptドキュメント - Dateオブジェクト](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)