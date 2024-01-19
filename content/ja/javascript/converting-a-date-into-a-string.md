---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を文字列に変換することとは、日付オブジェクトを人間が読むことができるフォーマットの文字列に変換することです。この操作は、ユーザーに日付情報を表示したり、ファイル名として使うために行われます。

## 方法と例：

日付を文字列に変換する基本的な方法を以下のコードで示します：

```Javascript
let date = new Date(); 
let dateAsString = date.toString(); 
console.log(dateAsString); 
```

これは次のような出力を提供します：

```Javascript
“Wed Sep 15 2021 12:30:00 GMT+0800 (Japan Standard Time)” 
```

このコードは現在の日付と時刻を新しい日付オブジェクトとして作成し、その日付オブジェクトを文字列に変換します。出力される文字列には曜日、月、日、年、現在の時刻、タイムゾーンが含まれます。

## 深層探訪:

過去、JavaScriptで日付を扱う方法は、`toString()`メソッドの他に`toDateString()`や`toTimeString()`などのメソッドもありました。これらのメソッドは、特定の部分（日付や時刻）だけを抽出するのに便利です。

しかし、現代のJavaScriptでは、`toLocaleString()`, `toLocaleDateString()`, `toLocaleTimeString()`などが導入され、異なる言語や地域のフォーマットに対応する能力が追加されました。

以下に異なる方法を示します:

```Javascript
let date = new Date();
console.log(date.toLocaleDateString('ja-JP'));
console.log(date.toLocaleTimeString('ja-JP'));
```

これらの方法は、適切に地域化された日付と時刻の文字列を提供します。

## 参考資料:

- [Mozilla Developer Network Docs (MDN) - Date](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript Date Reference - W3Schools](https://www.w3schools.com/jsref/jsref_obj_date.asp)
  
これらのリンクは、JavaScriptの日付の変換と操作に関する詳細な情報を提供します。