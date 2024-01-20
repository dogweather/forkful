---
title:                "将字符串大写"
html_title:           "Elixir: 将字符串大写"
simple_title:         "将字符串大写"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 什麼 & 為什麼？
首字母大寫字串是指將文字串中的每個單詞的首字母變成大寫。程序員這麼做主要是為了格式設定和權限管理。

# 如何跑？
用Elixir語言完成首字母大寫字串的功能，你可以用到\`String.capitalize/2\`這個函數。
```Elixir
IO.puts String.capitalize("elixir")
```
輸出結果就會是：
```
Elixir
```

# 更深層次的探索
首字母大寫字串的概念可以追溯至古老的打字機時代，為了讓文字更清楚易讀，人們將重要的字詞首字母大寫。在Elixir中，除了`String.capitalize/2`我們還可以用`String.upcase/1`和`String.downcase/1`結合起來創造首字母大寫字串的功能。
例如：
```Elixir
IO.puts String.upcase(String.slice("elixir", 0)) <> String.slice("elixir", 1..-1)
```
這樣輸出結果還是會是：
```
Elixir
```
這兩種方法的實現，底層都依賴於Erlang的字串操作函數。

# 更多資源
如果你想深入學習Elixir的String模型和函數操作，我推薦給你下面幾個是學習資源：
- [Elixir 官方String模組文檔](https://hexdocs.pm/elixir/String.html)
- [Erlang 字串操作函數列表](http://erlang.org/doc/man/string.html)
- [Elixir 學習手冊:字串操作](https://elixirschool.com/zh-hans/lessons/basics/strings/)