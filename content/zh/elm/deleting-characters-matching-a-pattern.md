---
title:                "删除匹配模式的字符。"
html_title:           "Elm: 删除匹配模式的字符。"
simple_title:         "删除匹配模式的字符。"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 為什麼

有時候我們需要刪除一些符合特定模式的字符，這可能是為了過濾敏感信息或者整理數據，讓它更容易處理。在Elm中，我們可以使用一些內置函數來達到這個目的。

## 如何進行

首先，我們需要定義一個需要處理的字符串，比如："Hello, World!"，然後我們需要使用適合的函數來刪除指定模式的字符。在Elm中，我們可以使用`String.replace`函數來實現這一點。

```Elm
String.replace "o" "" "Hello, World!" -- "Hell, Wrld!"
```

這個函數接受三個參數，第一個參數是要被替換的字符，第二個參數是要替換成的字符（這裡我們傳入了空字符串），第三個參數是要進行替換操作的字符串。使用這個函數，我們可以刪除所有的 "o" 字符。

如果我們想要刪除一組字符，比如 "l" 和 "d"，我們可以使用`String.replaceMany`函數。

```Elm
String.replaceMany ["l", "d"] "" "Hello, World!" -- "Heo, Wor!"
```

此外，如果我們想要刪除一個字符串中所有的數字，我們可以使用`String.filter`函數。

```Elm
String.filter Char.isDigit "H3ll0, W0rld!" --"Hell, World!"
```

在這個例子中，我們用`Char.isDigit`作爲過濾條件，所有的數字將被刪除。

## 深入了解

除了上面提到的三個內置函數，Elm還提供了許多其他用於處理字符的函數，比如`String.map`、`String.foldl`等。你可以通過[官方文檔](https://package.elm-lang.org/packages/elm/core/latest/String)來了解更多關於這些函數的用法。

## 參考資料

- [Elm官方文檔](https://package.elm-lang.org)

## 參見

F# Programming in Mandarin for Beginners: https://github.com/Akryum/fable-conf-2020.