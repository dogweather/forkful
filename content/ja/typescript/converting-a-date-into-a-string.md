---
title:    "TypeScript: 日付を文字列に変換する"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

# なぜ日付を文字列に変換するのか？

プログラミングでは、日付を扱う場面が多々あります。しかし、時には日付を文字列に変換する必要があります。例えば、データベースに日付を保存する際や、画面に表示する際に文字列に変換することが求められるかもしれません。そこで今回は、TypeScriptを使って日付を文字列に変換する方法をご紹介します。

## 変換方法

まず、Dateオブジェクトを使用します。これは、日付や時刻を表すオブジェクトであり、TypeScriptでも同様に使用することができます。以下のコードでは、現在の日付を取得し、文字列に変換する方法を示します。

```TypeScript
const today = new Date();
const strDate = today.toDateString();
console.log(strDate); 
``` 
このコードを実行すると、現在の日付が"月 日 年"の形式で表示されます。

また、toDateString()の代わりに、toTimeString()を使用することで、時刻を文字列に変換することもできます。さらに、toISOstring()を使用することで、ISO 8601形式の文字列に変換することもできます。

```TypeScript
const today = new Date();
const time = today.toTimeString();
const isoDate = today.toISOString();
console.log(time); // "時:分:秒"
console.log(isoDate); // "年-月-日T時:分:秒.000Z"
```

日付を指定して文字列に変換することも可能です。以下のように、年月日を指定してDateオブジェクトを作成し、toDateString()を使うことで、指定した日付を文字列に変換することができます。

```TypeScript
const specifiedDate = new Date(2021, 7, 30);
const strDate = specifiedDate.toDateString();
console.log(strDate); // "8月 30 2021"
```

## 深堀り

Dateオブジェクトでは、年や月、日だけでなく、時間や分、秒などの情報も取得することができます。また、Timezoneを考慮した日付の取得や変換など、さまざまな方法があります。詳しくは、公式ドキュメントを参照することをおすすめします。

## 参考リンク

[MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

[TypeScript Handbook: Date](https://www.typescriptlang.org/docs/handbook/intro-to-d-ts.html#date)

# 関連リンク

[TypeScriptで文字列を数値に変換する方法](https://example.com/string-to-number)