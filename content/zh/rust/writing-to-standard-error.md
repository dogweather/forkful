---
title:                "使用标准错误写入"
html_title:           "Rust: 使用标准错误写入"
simple_title:         "使用标准错误写入"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

＃＃ 什麼和為什麼？
寫到標準錯誤是指將程式執行中遇到的錯誤消息輸出到終端或日誌文件中，而不是直接輸出到終端使用者看到的標準輸出流中。這樣做的原因是為了讓程序員能夠更容易地追蹤和調試程序中的錯誤。

＃＃ 如何：
```Rust
fn main() {
    eprintln!("這是一個錯誤訊息。");
}
```
在這個例子中，我們使用```eprintln!``` 宏將一個錯誤訊息輸出到標準錯誤流中。使用 ```eprintln!``` 而不是 ```println!``` 的原因是，後者會將輸出信息輸出到標準輸出流中，而不是我們想要的標準錯誤流。

```
這是一個錯誤訊息。
```
這是程序執行時輸出的錯誤訊息，並且會顯示在終端上。

＃＃ 深入探討：
寫入標準錯誤流的概念起源於 Unix 系統，它是一種標準化的機制，用於將不同程序的錯誤信息報告給使用者。除了在終端上顯示錯誤信息外，程序員還可以將這些信息輸出到日誌文件中，以便稍後查看。

除了使用 ```eprintln!``` 宏之外，程序員也可以使用 ```std::io::stderr()``` 函數將信息寫入標準錯誤流。雖然可以直接通過標準錯誤流寫入錯誤信息，但使用 Rust 的高級標準庫函數可以讓代碼更加健壯和易於維護。

＃＃ 參考資料：
- [std::io::stderr()](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Unix file descriptor](https://en.wikipedia.org/wiki/File_descriptor)