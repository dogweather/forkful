---
title:    "Gleam: 删除匹配模式的字符"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

香港首次舉行的Gleam編程博客：刪除符合模式的字符

## 為什麼
刪除符合模式的字符是Gleam中一項非常有用的功能。通常，當我們處理數據時，可能會遇到一些不需要的字符，這些字符會干擾我們對數據的操作。使用Gleam的刪除字符功能，可以輕鬆地將這些不需要的字符從數據中刪除，讓數據更加乾淨和易於處理。

## 如何使用
Gleam提供了一種方便的方法來刪除符合特定模式的字符。讓我們來看一個例子，假設我們有一個字符串列表，其中包含一些數字和一些字母。我們想要刪除列表中所有的字母，只保留數字。使用Gleam的刪除字符功能，我們可以這樣做：

```Gleam
input = ["a", "1", "b", "2", "c", "3"]

output = Enum.filter(input, fn (char) ->
  Char.is_numeric(char)
end)

IO.inspect(output) # ["1", "2", "3"]
```

如上所示，我們使用`Enum.filter`函數來過濾列表中的字符。在過濾函數中，我們使用`Char.is_numeric`來判斷每個字符是否是數字，只保留符合條件的字符。最後，我們使用`IO.inspect`來輸出刪除後的列表。

## 深入了解
除了`Char.is_numeric`外，Gleam還提供了許多其他函數來刪除不需要的字符。例如，`Char.is_whitespace`用於判斷是否是空格字符，`Char.is_uppercase`用於判斷是否是大寫字母。使用這些不同的函數，我們可以更精確地根據需要刪除想要的字符。同時，Gleam還提供了通用的`Char.is_pattern`函數，可以根據自定義的正則表達式來判斷是否符合特定模式。

## 參考資料
- [Gleam官方文檔](https://gleam.run/documentation/stdlib/char/)
- [Gleam程式語言：快速簡捷的編程體驗](https://gleam.run/documentation/tutorial/)
- [正則表達式的基礎課程](https://regexone.com/)