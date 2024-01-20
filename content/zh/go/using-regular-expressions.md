---
title:                "使用正则表达式"
html_title:           "Go: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/using-regular-expressions.md"
---

{{< edit_this_page >}}

# 甚麼 & 為什麼？
使用正則表達式是一種在程式設計中常用的技術。它可以讓我們更有效地搜尋、替換和檢查字串。許多程式設計師喜歡使用正則表達式，因為它們可以讓他們更快速地處理複雜的字串操作。

# 如何：
接下來我們將使用 Go 語言來示範如何使用正則表達式。首先，我們需要導入 "regexp" 套件來使用正則表達式功能。然後，我們可以使用 ```regexp.Compile``` 函式來編譯我們需要的正則表達式，並使用 ```MatchString``` 函式來確認字串是否符合正則表達式的規則。

```Go
import "regexp"

// 編譯正則表達式
phoneRegExp := regexp.MustCompile(`\d{4}-\d{6}`)

// 確認字串是否符合規則
fmt.Println(phoneRegExp.MatchString("0911-123456")) // 輸出：true
fmt.Println(phoneRegExp.MatchString("abc123")) // 輸出：false
```

# 深入了解：
正則表達式是在 20 世紀 50 年代由數學家 Stephen Cole Kleene 發明的，它的概念是基於正規語言的理論。除了在程式設計中常用的字串操作外，正則表達式也被應用在文本搜尋、資料驗證等領域。除了 Go 語言之外，其他程式語言也都支援使用正則表達式，如 Python、JavaScript 等。

如果你想要更深入地了解正則表達式的用法，可以參考下列連結：

- [Go 語言官方正則表達式文件](https://golang.org/pkg/regexp/)
- [正規語言基礎知識](https://zh.wikipedia.org/zh-tw/%E6%AD%A3%E5%88%99%E8%AF%AD%E8%A8%80)
- [Regex101：正則表達式測試器](https://regex101.com/)

# 相關資源：
- [Go 語言官方網站](https://golang.org/)
- [Awesome Go：收藏了許多 Go 語言相關的資源](https://github.com/avelino/awesome-go)