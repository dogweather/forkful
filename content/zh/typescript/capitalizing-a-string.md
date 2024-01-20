---
title:                "将字符串大写"
html_title:           "TypeScript: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什麼 & 為什麼?
正規化字符串是把一個字符串的第一個字符轉換成大寫。編程時，這常常用來清洗數據或增強用戶界面的友好性。

## 如何操作:
以下是在TypeScript中實現字串變大寫的編程例程:
 ```TypeScript
 function capitalizeString(str: string): string {
   return str.charAt(0).toUpperCase() + str.slice(1);
 }
 
 console.log(capitalizeString("hello world!"));  //輸出:"Hello world!"
 ```

## 深入了解:
1. 歷史背景：字符串轉換功能在許多程式語言之初就被建立，因為他們經常被用於基本數據操作和處理。
2. 替代方案：有許多函式庫提供了字串首字母大寫的方法，例如lodash和Ramda。儘管如此，原生的JavaScript仍然可以很容易地實現字符串首字母大寫。
3. 實現細節：首先，我們使用`charAt(0)`來獲取字符串的第一個字符，然後使用`toUpperCase()` 函數進行大寫轉換。接著，使用`slice(1)`取得從第2個字符到字符串末尾的所有字符，並將其與大寫的首字符相連接。

## 參考資料:
1. MDN字符串方法資料: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
2. TypeScript文檔: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
3. Ramda庫: [https://ramdajs.com/](https://ramdajs.com/)
4. Lodash庫: [https://lodash.com/](https://lodash.com/)