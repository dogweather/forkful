---
title:                "Gleam: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 為什麼
轉換字串為小寫可能是編程中常見且有用的任務。例如，您可能需要將使用者輸入的字串規範化以便進行比較，或者在處理數據時需要統一格式。

## 如何進行
如果您使用 Gleam 編程語言，將一個字串轉換為小寫相當簡單。以下是一個示例代碼：

```Gleam
let original_string = "Hello Gleam"
let lower_case_string = String.to_lower(original_string)
```

使用 `String.to_lower` 函數將一個字串轉換為小寫。如果您想要轉換為大寫，則可以使用 `String.to_upper` 函數。

下面是上面代碼的輸出結果：

```Gleam
lower_case_string: "hello gleam"
```

## 深入探討
當您使用 `String.to_lower` 函數時，Gleam 實際上是使用 [Unicode 轉換算法](https://unicode.org/reports/tr21/) 來進行轉換。這意味著它不僅僅支持英文字母的大小寫轉換，還支持多種語言的轉換。

此外，您還可以使用 [String.split](https://gleam.run/core/string#split) 函數將一個字串分割為多個子字串，然後對每個子字串使用 `String.to_lower` 函數。

## 參考資料
- Gleam 編程語言官方網站：https://gleam.run/
- Unicode 轉換算法官方文檔：https://unicode.org/reports/tr21/
- Gleam `String` 模塊文檔：https://gleam.run/core/string