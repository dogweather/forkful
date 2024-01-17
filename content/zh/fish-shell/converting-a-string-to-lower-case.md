---
title:                "将字符串转换为小写"
html_title:           "Fish Shell: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什麼 & 為什麼？

將一個字符串轉換為小寫是指將字符串中的字母都轉換為小寫形式。程式設計師會做這件事是因為在編程時，有時候需要比較字母的大小寫，而轉換為小寫可以讓比較更精確。

## 如何：

```
Fish Shell 中，轉換字符串為小寫的語法是：

$ echo "Hello, World!" | tr '[A-Z]' '[a-z]'

這樣就可以把字符串轉換為小寫。

輸出結果：hello, world!

```

## 深入探討：

1. 歷史背景：在舊的電腦系統中，只有大寫字母，而小寫字母是在後來才加入的。所以在一些舊的程式設計語言中，標示大小寫是很重要的，而轉換字符串為小寫可以方便比較字母。

2. 其他方法：除了使用 Fish Shell 中的 tr 指令，還可以使用其他程式語言中的函數或方法來轉換字符串為小寫，如 Python 中的 lower() 函數、Java 中的 toLowerCase() 方法等。

3. 實現細節：轉換字符串為小寫的過程其實就是將字符串中的大寫字母替換為相應的小寫字母。

## 請參考：

- Fish Shell 官方文檔：https://fishshell.com/docs/current/
- tr 指令詳細使用方法：https://zh.wikipedia.org/wiki/Tr_(Unix)
- 認識舊的電腦系統：https://www.dropbox.com/s/2ep8mhwk3v4b01r/Turning%20Up%20the%20Heat%20-%20Two%20Centuries%20of%20Industrial%20Development.pdf