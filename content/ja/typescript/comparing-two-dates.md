---
title:                "日付の比較"
html_title:           "TypeScript: 日付の比較"
simple_title:         "日付の比較"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することのメリットは何でしょうか？ 日付を比較することで、特定の日付が過去、現在、または未来のどの時点に位置するかを判断することができます。例えば、クーポンやイベントの有効期限をチェックする際に、日付を比較することで、有効期限が切れたかどうかを簡単に確認できます。

## 方法

日付を比較する最も簡単な方法は、`Date`クラスを使用することです。以下のように、比較したい二つの日付を作成し、`>`、`<`、`==`などの比較演算子を使用します。
```TypeScript
let firstDate = new Date(2021, 3, 15); 
let secondDate = new Date(2021, 3, 20); 

console.log(firstDate > secondDate); // false
console.log(firstDate < secondDate); // true
console.log(firstDate == secondDate); // false
```
このコードは、`firstDate`が`secondDate`よりも前の日付かどうかを判断します。また、`==`演算子を使用しても、二つの日付が完全に一致しない場合は`false`が返されます。 

## ディープダイブ

日付を比較する際には、時間や時差などにも注意が必要です。`Date`クラスでは、日付だけでなく、時刻やタイムゾーンも操作することができます。また、日付を比較する際には、`getTime()`メソッドを使用して、日付をミリ秒数に変換してから比較することが推奨されています。 

例えば、以下のコードでは、現在の日付と未来の日付を比較し、タイムゾーンを考慮して値を返します。
```TypeScript
let currentDate = new Date(); 
let futureDate = new Date(2021, 3, 15); 

console.log(currentDate < futureDate); // true
console.log(currentDate.getTime() < futureDate.getTime()); // false
```

## 参考リンク

- [MDN Web Docs: Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript: Working with Dates](https://www.typescriptlang.org/docs/handbook/working-with-dates.html)
- [W3Schools: JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)

## 参照

- これ以外にも、日付を比較する方法は様々あります。自分に合った方法を見つけて、より使いやすいコードを書いてみてください。