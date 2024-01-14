---
title:                "Javascript: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

なぜ私たちはJavaScriptプログラミングにおいて現在の日付を取得するのか、その理由を知りたいと思いませんか？JavaScriptには日付を取得する簡単な方法があり、便利なアプリケーションを開発する上でも重要なスキルです。この記事では、現在の日付を取得する方法とその背景について紹介します。

## 方法

JavaScriptでは、現在の日付を取得するために`Date()`オブジェクトを使用します。以下のコードを参考にしてください。

```Javascript 
// 現在の日付を取得する
const currentDate = new Date();
console.log(currentDate); //2021-07-31T08:23:36.004Z

// 年、月、日を取得する
const year = currentDate.getFullYear();
const month = currentDate.getMonth();
const date = currentDate.getDate();
console.log(year); // 2021
console.log(month); // 6 (Javascriptでは月は0から始まるため、7月であっても6と表示されます)
console.log(date); // 31
```

`new Date()`を使用すると、現在の日時が表されるオブジェクトが作成されます。ここから、`getFullYear()`、`getMonth()`、`getDate()`を使用してそれぞれ年、月、日を取得することができます。

また、現在の時刻を取得するには`getHours()`、`getMinutes()`、`getSeconds()`を使用します。以下のコードを参考にしてください。

```Javascript
// 現在の時刻を取得する
const currentDateTime = new Date();
const hour = currentDateTime.getHours();
const minute = currentDateTime.getMinutes();
const second = currentDateTime.getSeconds();
console.log(hour); // 17
console.log(minute); // 26
console.log(second); // 27
```

## ディープダイブ

JavaScriptには`Date()`オブジェクト以外にも日付を取得する方法があります。例えば、`new Date()`の代わりに`Date.now()`を使用すると、ミリ秒単位の現在の日時を取得することができます。また、`toLocaleDateString()`を使用すると、ブラウザのロケーションに合わせた日付を取得することができます。

日付を取得する際には、タイムゾーンの考慮やブラウザごとの差異についても知る必要があります。さらに詳しく知りたい方は、[MDN web docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)を参考にしてください。

## 詳しくはこちらを参照

[JavaScriptで日付を取得する方法](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)

[JavaScriptで使えるDateオブジェクトのメソッド一覧](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date#methods)

[Date.now()を使用して現在の日時を取得する](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date/now)

---

## 参考リンク

[MDN web docs](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)

[Date オブジェクトのリファレンス](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date#Methods) 

[JavaScripでの日時操作について知る](https://tech.recruit-mp.co.jp/front-end/post-9887/)