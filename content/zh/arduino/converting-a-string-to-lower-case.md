---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 字串轉換成小寫：Arduino程式語言的導引

## 什麼與為何?

字串轉換成小寫是變換所有文本字符為其小寫型態。程序員這样做是因為程式語言往往對字元大小寫敏感。

## 如何做：

下面的Arduino程式碼是將字串轉換為小寫型態。

```Arduino
String my_string = "HELLO WORLD!";
my_string.toLowerCase();
```

這將會使`my_string`的內容變為"hello world!"。

再來一個範例：

```Arduino 
String my_string = "Ni Hao Ma";
my_string.toLowerCase();
```

這將產生“ni hao ma”。

## 深入瞭解：

轉換字串為小寫的觀念源於最早的電腦時代，人們需要一種方式來確保電腦正確理解人類語言。有了字母的統一寫法,防止因大小寫不同而產生的錯誤。

當不可能使用`toLowerCase()`函數另一個程式範例可能會用途c型態的函式`tolower()`。這也是個有效的替代方法。

Arduino的`toLowerCase()`函數是利用ASCII碼來實現的，ASCII碼中每個大寫字母和其對應的小寫字母之間的值差為32。

## 參閱連結：

1. Arduino String類型參考：https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/

2. ASCII對照表：http://www.asciitable.com/

3.   C 語言 tolower() 函數用法：https://www.itread01.com/content/1546247849.html