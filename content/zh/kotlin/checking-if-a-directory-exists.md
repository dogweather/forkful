---
title:                "检查目录是否存在。"
html_title:           "Kotlin: 检查目录是否存在。"
simple_title:         "检查目录是否存在。"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

什麼是目錄存在性，為什麼程式設計師需要檢查它？

##什麼是目錄存在性？
當我們在寫程式時，有時會需要檢查一個目錄是否存在。目錄存在性指的是在檔案系統中，我們可以通過程式來檢查一個目錄是否已經被創建了。這是一個非常基本的操作，但卻非常重要，因為它可以幫助我們確保在操作文件或資料前，目錄已經存在。

##為什麼程式設計師需要檢查目錄存在性？
在我們撰寫程式時，有時會需要在程式執行期間創建新的目錄，或是讀取已經存在的目錄中的檔案。如果在這個過程中，我們沒有檢查目錄是否存在，就有可能會出現錯誤或程式崩潰的情況。因此，檢查目錄存在性可以幫助我們確保程式的順利執行。

##如何檢查目錄存在性？
以下是一個使用Kotlin語言檢查目錄存在性的程式碼範例：

```Kotlin
import java.io.File

fun main(args: Array<String>) {
    val path = "path/to/directory"
    val directory = File(path)

    if (directory.exists()) {
        println("Directory exists!")
    } else {
        println("Directory does not exist.")
    }
}
```

執行此程式後，如果該路徑下的目錄存在，則會印出"Directory exists!"。如果該目錄不存在，則會印出"Directory does not exist."。

##深入了解
###歷史背景
在過去，對目錄存在性的檢查是非常必要的操作。因為當時的檔案系統並沒有像現在這樣的功能，因此程式設計師必須透過程式來檢查目錄是否存在。但隨著科技的進步，檔案系統已經能夠自動創建目錄，因此現在對目錄存在性的檢查已經不再像以前那麼重要。

###其他做法
除了使用程式來檢查目錄是否存在，也可以透過命令列指令來進行檢查。在Windows系統中，可以使用dir指令來列出目錄下的所有檔案。在Mac或Linux系統中，則可以使用ls指令。如果目錄不存在，這些指令都會顯示錯誤訊息。

###實作細節
在Kotlin中，我們可以使用File類別的exists()方法來檢查目錄是否存在。這個方法會回傳一個布林值，如果目錄存在則會回傳true，否則回傳false。我們也可以使用isDirectory()方法來進一步確認是否為一個目錄。

##相關資源
如果你想進一步了解有關檔案系統操作的知識，可以參考以下資源：

- [Java Tutorials - Working with Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Kotlin Documentation - File handling](https://kotlinlang.org/docs/reference/java-interop.html#file-handling)
- [Linux Command Line - File Management](https://www.linuxcommand.org/lc3_lts0070.php)