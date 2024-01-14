---
title:    "Kotlin: 开始一个新项目"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 為什麼

在計算機科學和軟件開發領域，建立新專案是很常見的。通常，人們會對新技術、新想法或者解決特定問題的方式感興趣。另外，也有可能是為了學習和提升技能而開始新專案。

## 如何

假設你已經設置好了Kotlin的開發環境，讓我們來看看如何開始一個新的專案。

首先，創建一個新資料夾作為專案的根目錄。

接著，在這個資料夾中創建一個Kotlin腳本檔案，並將它命名為`main.kt`。

在`main.kt`的開頭，輸入以下程式碼：

```Kotlin
fun main() {
    println("Hello World!")
}
```

這是一個最基本的Kotlin程式，它將會印出一個簡單的消息。

為了執行這個程式，只需要在終端機中輸入以下指令：

```Kotlin
kotlinc main.kt -include-runtime -d main.jar
```

這個指令會將程式碼編譯並打包成一個可執行的jar檔案。

最後，輸入以下指令來執行這個程式：

```Kotlin
java -jar main.jar
```

你應該可以在終端機中看到`Hello World!`這個訊息被印出來。

恭喜，你已經成功建立了一個新的Kotlin專案！

## 深入探究

通常，在開始新專案之前，你需要考慮一些重要的因素。

首先，你需要明確瞭解專案的目標和目的。這會幫助你制定適合的計劃和設計。

接著，你需要選擇適合的開發工具和框架。Kotlin有許多優秀的框架可以幫助你更有效率地開發專案。

最後，你需要考慮專案的架構和設計模式。適合的架構和設計模式能夠讓專案更易於維護和擴展。

## 參考資料

- [Kotlin官方網站] (https://kotlinlang.org/)
- [Kotlin中文說明文件] (https://kotlinlang.org/docs/reference/)