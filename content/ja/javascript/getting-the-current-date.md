---
title:                "「現在の日付の取得」"
html_title:           "Javascript: 「現在の日付の取得」"
simple_title:         "「現在の日付の取得」"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 今日の日付を取得するには？

現代のプログラミングにおいて、日付 (年/月/日) を取得することは非常に重要です。それにはいくつかの理由があります。例えば、プログラマーはユーザーにとって最新の情報を表示したいと考えることができます。また、日付を取得することで、ソフトウェアの処理時間やトラブルシューティングにも役立ちます。

## 方法：

```Javascript
const today = new Date(); // 現在の日付を取得
console.log(today); // 出力例: Thu Oct 14 2021 09:00:00 GMT+0900 (Japan Standard Time)

const year = today.getFullYear(); // 現在の年を取得
console.log(year); // 出力例: 2021

const month = today.getMonth() + 1; // 現在の月を取得
console.log(month); // 出力例: 10

const date = today.getDate(); // 現在の日を取得
console.log(date); // 出力例: 14
```

## 詳細を調べる：

日付を取得する方法は時代とともに進化してきましたが、現在のJavascriptのバージョンでは、```Date```オブジェクトを使用することが最も一般的です。他の方法として、多くのライブラリやフレームワークが提供している日付取得用の関数もありますが、それぞれの技術によって使用方法が異なるため、Javascriptの基本的な知識を学ぶことが重要です。

また、日付を取得するための```Date```オブジェクトには、様々なメソッドが存在します。例えば、```getFullYear()```や```getMonth()```、```getDate()```の他にも、```getDay()```や```getTime()```などがあります。詳しい情報は公式ドキュメントを参照することができます。

## 関連リンク：

- [MDN Dateオブジェクト](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScriptで日付を取得する方法](https://webllica.com/javascript/getting-current-date-in-javascript/)
- [JavaScriptについての日本語ドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript)