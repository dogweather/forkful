---
title:                "创建一个临时文件"
html_title:           "Clojure: 创建一个临时文件"
simple_title:         "创建一个临时文件"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

「什麼是臨時文件？為什麼程式設計師要這麼做？」
臨時文件是程式設計師在編寫程式時暫時創建的一個檔案。程式設計師通常會使用臨時文件來儲存暫時計算結果或中間數據，在程式執行完成後會被自動刪除。

「如何創建臨時文件？」
Clojure提供了一個內建的函數```java.io.File/createTempFile```來創建臨時文件。你可以指定臨時文件的前綴和後綴，也可以選擇在指定的路徑下創建臨時文件。以下是一個創建臨時文件的範例：

```
(def temp-file (java.io.File/createTempFile "prefix" "suffix"))
```

創建完成後，臨時文件會被指定的路徑下命名為"prefix[random number]suffix"，並返回一個代表該臨時文件的 ```java.io.File``` 物件。

「深入探討」
創建臨時文件是一個常見的程式設計技巧，它可以幫助我們處理暫時的數據或結果，同時保持程式的乾淨和可讀性。除了使用Clojure提供的```java.io.File/createTempFile```函數外，還可以使用其他語言或工具來創建臨時文件，例如在Shell腳本中使用```mktemp```命令。

「參考資料」
- [Clojure官方文檔：java.io.File](https://clojure.org/reference/java_interop#_file)
- [mktemp命令說明](https://www.man7.org/linux/man-pages/man1/mktemp.1.html)