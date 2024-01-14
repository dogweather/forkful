---
title:    "Arduino: 使用正则表达式"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

談Arduino正則表達式的使用

## 為什麼要使用正則表達式?

正則表達式是一種強大的工具，可以協助我們在程式中搜尋和處理文字資料。它可以幫助我們更有效率地處理複雜的字串操作，同時也可以讓我們的程式碼更容易閱讀和維護。

## 如何使用正則表達式?

要在Arduino中使用正則表達式，我們需要使用到一個稱為Regexp的函式庫。讓我們來看一個簡單的範例，假設我們要檢查一個字串是否包含數字：

```Arduino
#include <Regexp.h>

void setup(){
    Serial.begin(9600);
    String str = "Hello123World";
    Regexp re("\\d+"); //建立一個正則表達式物件，可以檢查是否包含一個以上的數字
    if (re.search(str)) {
        Serial.println("String contains numbers.");
    } else {
        Serial.println("String does not contain numbers.");
    }
}

void loop(){

}
```

在這個範例中，我們使用了Regexp函式庫中的search()函式來檢查字串中是否包含數字。如果字串中有數字，就會輸出 "String contains numbers."，否則就會顯示 "String does not contain numbers."。

除此之外，在使用正則表達式時還可以使用不同的符號來表示不同的匹配規則，例如：

- "." 代表任意一個字元
- "*" 代表零個或多個前一個符號
- "+" 代表一個或多個前一個符號
- "^" 代表行首
- "$" 代表行尾

你可以透過[Regexp函式庫的文件](https://www.arduino.cc/en/Reference/Regexp)來學習更多關於正則表達式的使用方法。

## 深入了解正則表達式

正則表達式的語法和符號並不是那麼容易理解，因此建議在開始使用之前先閱讀一些相關的教學文章和練習。另外，你也可以透過一些線上的正則表達式測試工具來測試你的表達式是否符合預期的規則。

另外，除了Regexp函式庫之外，還有其他一些可以在Arduino中使用的正則表達式函式庫，例如[Regexp](https://github.com/nickgammon/Regexp)或[SimpleRegexp](https://github.com/msaunby/SimpleRegexp)，你可以依據自己的需要和程式的複雜度來選擇適合的函式庫。

## 參考資料

- [正則表達式維基百科](https://zh.wikipedia.org/wiki/正則表達式)
- [Arduino官方正則表達式函式庫文件](https://www.arduino.cc/en/Reference/Regexp)
- [正則表達式測試工具](https://regex101.com/)

## 參見

- [Arduino官方文件](https://www.arduino.cc/en)
- [Arduino套件管理器](https://github.com/arduino/Arduino/wiki/Installing-third-party-boards-package)