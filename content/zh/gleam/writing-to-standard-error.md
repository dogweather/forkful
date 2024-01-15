---
title:                "写入标准错误"
html_title:           "Gleam: 写入标准错误"
simple_title:         "写入标准错误"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 為什麼

當你在編寫程式時，你可能會遇到錯誤或者需要輸出一些訊息給使用者。使用標準錯誤來寫入這些訊息可以使得程式更加有效率和方便。

## 如何操作

要寫入標準錯誤，你需要使用```gleam/io```模組中的```eprint```函數。例如：

```Gleam
import gleam/io eprint

fn main() {
  eprint("這是標準錯誤訊息")
}
```

輸出將會是：

```
這是標準錯誤訊息
```

## 深入瞭解

雖然標準錯誤通常用於輸出錯誤訊息，但它其實也可以用於其他情況。你可以使用```eprint```來輸出任何類型的訊息，並且它也可以與其他函數和宏一起使用。此外，你也可以使用```println!```來輸出至標準輸出。

## 參考資料

- 標準錯誤的更多資訊：https://gleam.run/articles/errors
- ```gleam/io```模組的官方文件：https://gleam.run/std/io