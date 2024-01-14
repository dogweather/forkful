---
title:                "TypeScript: 日付の取得方法"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

日付を取得することの利点を教えます。日付は、多くのプログラミングに欠かせない情報です。例えば、予約システムやタイムスタンプ、タイムゾーンの設定など、多くのアプリケーションが日付を使っています。今回は、TypeScriptを使って日付を取得する方法をご紹介します。

## 方法

まずは、日付を取得する前に使用するDateオブジェクトを作成する必要があります。そのために、以下のコードを使用します。

```TypeScript
const currentDate: Date = new Date();
```

上記のコードでは、現在のタイムゾーンを使用してDateオブジェクトが作成されます。もし、特定のタイムゾーンを使用したい場合には、以下のようにすることができます。

```TypeScript
const currentDate: Date = new Date("2021-08-25T09:00:00.000+09:00");
```

次に、年、月、日などの特定の情報を取得する方法をご紹介します。下記のコードを参考にしてください。

```TypeScript
const year: number = currentDate.getFullYear();
const month: number = currentDate.getMonth() + 1;
const day: number = currentDate.getDate();
```

上記のコードでは、Dateオブジェクトから現在の年、月、日を取得しています。JavaScriptのDateオブジェクトには、さまざまなメソッドがあり、詳細については公式ドキュメントを参照してください。

## 深く掘り下げる

Dateオブジェクトには、タイムスタンプやタイムゾーン、月初めや月末など、さまざまな情報を取得するためのメソッドがあります。また、Dateオブジェクトの値を比較することで、日付の計算を行うこともできます。

さらに、外部ライブラリを使用することで、より高度な日付処理を行うこともできます。Moment.jsやDay.jsなど、多くのライブラリが日付処理に特化しています。

## See Also

- [JavaScript Dateオブジェクトのドキュメント](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js公式サイト](https://momentjs.com/)
- [Day.js公式サイト](https://day.js.org/)