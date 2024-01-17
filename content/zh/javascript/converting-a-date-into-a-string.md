---
title:                "将日期转换为字符串"
html_title:           "Javascript: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什麼和為什麼？

日期轉換成字串是將日期數據轉換為可讀的文字格式。程式設計師做這件事的原因是為了讓日期更容易閱讀和理解，以及方便日期在不同的程式語言和格式之間轉換。

## 如何：

```
// 使用JavaScript中的內建函式將日期轉換為字串
let today = new Date(); // 創建一個日期物件
let dateString = today.toDateString(); // 使用toDateString()方法將日期轉換為字串
console.log(dateString); // 輸出："Sat Nov 14 2020"

// 使用第三方函式庫Moment.js將日期轉換為指定格式的字串
let today = new Date(); // 創建一個日期物件
let dateString = moment(today).format('MMMM Do YYYY'); // 使用format()方法將日期轉換為"Month Day Year"格式的字串
console.log(dateString); // 輸出："November 14th 2020"
```

## 深入探討：

日期轉換成字串其實是一項很常見的任務，在不同的程式語言和平台中都會有相應的方法或函式可以使用。除了使用內建函式或第三方函式庫，也可以自己寫程式碼來完成日期轉換的任務。不過需要注意的是，日期的格式會影響轉換結果，因此在使用時需確保日期格式的準確性。

## 參考資料：

- [日期物件(Date) - JavaScript | MDN](https://developer.mozilla.org/zh-TW/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - Parse, validate, manipulate, and display dates in JavaScript](https://momentjs.com/)