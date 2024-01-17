---
title:                "产生随机数"
html_title:           "Ruby: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 做什麼和為什麼？
隨機數字生成是指使用電腦程式來產生一系列看似無規律的數字。程序員之所以這麼做，是因為隨機數字在很多應用中都扮演著重要的角色。例如遊戲中的隨機地圖生成、加密技術中的密鑰生成等等。

## 如何：
首先，我們需要使用 `rand` 方法來產生隨機數字。這個方法可以接受一個參數，指定數字的上限。舉例來說，如果我們想要產生一個 1 到 10 之間的隨機數字，可以使用下面的程式碼：
```Ruby
rand(10) + 1
```
這個表達式的結果會是從 1 到 10 之間的一個整數。如果我們想要產生一個小數，可以使用 `rand` 方法的另一個版本 `rand()`，這個方法會產生介於 0 到 1 之間的一個小數，例如：
```Ruby
rand()
# 輸出可能是 0.8269941163249325 或 0.12461126459170875
```
此外，我們也可以利用 `range` 物件來產生一個指定範圍的隨機數，例如：
```Ruby
(1..5).to_a.sample
# 輸出可能是 1、2、3、4 或 5 的隨機數字
```

## 深入探討：
隨機數字在現代電腦技術中已經被廣泛運用，其歷史可追溯到 20 世紀。最初，程式員們使用物理現象來產生隨機數，例如擲骰子或翻硬幣。現在，我們通常使用電腦生成偽隨機數，即看似隨機但實際上是按規律生成的數字。除了 `rand` 方法外，Ruby 也提供了其他生成隨機數的方法，例如 `Random` 類別和 `SecureRandom` 模組。如果想要了解更多有關在電腦中生成隨機數的實作細節，可以參考以下資源。

## 參考資料：
- [Ruby `rand` 方法文檔](https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-rand)
- [Ruby `Random` 類別文檔](https://ruby-doc.org/core-3.0.0/Random.html)
- [Ruby `SecureRandom` 模組文檔](https://ruby-doc.org/stdlib-3.0.0/libdoc/securerandom/rdoc/SecureRandom.html)
- [如何在電腦中生成隨機數？](https://blog.csdn.net/On2hill/article/details/89094593) (中文文章)