---
title:    "TypeScript: 「日付を文字列に変換する」"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの利点は何でしょうか？日付を文字列に変換することで、日付をより見やすく、理解しやすくすることができます。また、特定の形式に日付を合わせることもできます。

## 使用方法

日付を文字列に変換するには、次のようなコードを使用します。

```TypeScript
const date = new Date();
const dateString = date.toString();
console.log(dateString);
```

この例では、現在の日付を "Jun 10 2021" の形式でコンソールに出力します。日付を変換する際には、Dateオブジェクトを作成し、toString()メソッドを使用します。また、他の形式で日付を表示する場合は、別のメソッドを使用する必要があります。

## 詳細について

JavaScriptでは、Dateオブジェクトを使用して日付を表します。このオブジェクトには、日付や時間を表示するためのさまざまなメソッドが用意されています。日付を文字列に変換するというのは、基本的にはDateオブジェクトのtoString()メソッドを使用することになります。これにより、国際標準時を元にした文字列表現が返されます。しかし、この方法ではすべての言語や地域で同じ形式で日付が表示されるわけではありません。そのため、他のメソッドを使用することで特定の形式に日付を合わせることができます。たとえば、toLocaleDateString()メソッドを使用すれば、地域や言語に合わせたフォーマットで日付を表示することができます。

## 参考リンク

[MDN - Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)  
[W3Schools - JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)  
[TypeScript Handbook - Date](https://www.typescriptlang.org/docs/handbook/dates-and-times.html)  

## 関連リンク