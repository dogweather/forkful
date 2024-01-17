---
title:                "通过大写设置字符串"
html_title:           "Swift: 通过大写设置字符串"
simple_title:         "通过大写设置字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 什麼是String大寫？: 
String大寫是指將一個字串中的所有字母轉換為大寫格式。程式設計師經常需要對字串進行操作，並且在需要進行字串比較時，大寫格式通常會更容易處理。

## 如何進行: 
```Swift
let str = "hello, world!"
print(str.uppercased())
```
透過使用`uppercased()`方法，我們可以將`str`的值轉換為大寫形式。執行程式後，將會在終端機中看到`HELLO, WORLD!`的輸出。

## 深入探討: 
- 歷史背景: 在舊的程式語言中，字符串操作是一個非常常見的任務，因此大多數語言都有提供字串轉換為大寫的方法。
- 替代方法: 除了使用`uppercased()`方法，我們也可以使用`capitalized()`方法，將字串的第一個字母轉換為大寫。
- 實作細節: 在Swift中，字串是一個結構(struct)類型，因此我們可以透過指定`str`的屬性`uppercased()`來將字串轉換為大寫格式。

## 相關資源: 
如果想進一步了解關於字串操作的資訊，可以參考下列連結：
- [String and Character - The Swift Programming Language (Swift 5.2)](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [String and Character Manipulation in Swift](https://medium.com/@happymishra66/string-and-character-manipulation-in-swift-1d1f01eb3e23)