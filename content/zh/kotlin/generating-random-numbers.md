---
title:                "生成随机数"
html_title:           "Kotlin: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

什麼 & 為什麼？
生成隨機數是指用電腦自動產生一系列隨機數字。程式設計師這麼做的原因很多，例如用於遊戲、模擬實驗、加密等等。

怎麼做？
```Kotlin
// 生成一個1到10之間的隨機整數
val randomNumber = (1..10).shuffled().first() 
println(randomNumber) // 輸出例子: 7 

// 生成一個0.0到1.0之間的隨機浮點數
val randomDouble = Random.nextDouble() 
println(randomDouble) // 輸出例子: 0.6258411670459416 
```

深入挖掘
生成隨機數的概念可以追溯到古代的占卜和骰子遊戲。在程式設計中，我們可以使用不同的演算法來產生隨機數，例如線性同餘法、反向方差法等等。也可以使用外部的隨機產生器，如硬體隨機數產生器或伺服器的時間戳記等。除了使用內建的Random類別，還可以使用第三方庫來產生更高品質的隨機數。

相關資源
如果想了解更多關於生成隨機數的知識，可以參考Kotlin官方文件中關於Random類別的說明。還有一些第三方庫，如Apache Commons、JDK Math類別等都有提供產生隨機數的方法。