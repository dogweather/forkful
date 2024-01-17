---
title:                "使用正则表达式"
html_title:           "Arduino: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 甚麼 & 為何做？

* Arduino 的最新版本可以很容易地使用 regular expressions，它是一種用來在字串中定義搜尋條件的方法。
* 程式員使用 regular expressions 可以讓他們更有效率地處理字串資料，並且達到更精準的搜尋結果。

## 如何：

```
Arduino code block
```

* 使用 ```re.search()``` 函式來尋找符合條件的字串，並回傳第一個符合的結果。
* 使用 ```re.findall()``` 函式來尋找符合條件的所有字串，並回傳一個列表。
* 使用 ```re.sub()``` 函式來替換字串中符合條件的部分，並回傳替換後的結果。

## 深入瞭解

* Regular expressions 最早是由美國科學家 Stephen Kleene 於 1950 年代提出的，並在 1960 年代被 Ken Thompson 和 Dennis Ritchie 實作在 Unix 系統中。
* 除了使用 regular expressions，程式員也可以使用其他方法（如字串處理函式）來達到相同的目的。
* 在 Arduino 中使用 regular expressions 需要先引入 ```#include <Regex.h>``` 標頭檔。

## 參考資料

* [Python 中的 regular expressions 介紹](https://docs.python.org/3/library/re.html)
* [Arduino 官方網站的 string 物件文件](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
* [Wikipedia 上關於 regular expressions 的詳細說明](https://en.wikipedia.org/wiki/Regular_expression)