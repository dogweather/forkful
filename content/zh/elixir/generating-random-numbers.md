---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
-生成随机数是生成不确定的编号过程。
-程序员生成随机数来测试函数，模拟偶然事件，或创建唯一的ID。

## 如何：
在Elixir中生成随机数，可以用`Enum.random/1`, `:rand.uniform/1` 和 `:rand.uniform/0` 方法。

```Elixir
iex> Enum.random(1..100) 
47

iex> :rand.uniform(5)  
2

iex> :rand.uniform()
0.44358461744572024
```

## 深度探讨
###历史背景
过去，Elixir使用了`:random`模块来生成随机数，但在Erlang 19中被`:rand`模块取代。
### 替代方法
你也可以使用 `:crypto.strong_rand_bytes/1` 和 `:binary.decode_unsigned/1` 来创建一个更安全的随机数。

```Elixir
iex> :binary.decode_unsigned(:crypto.strong_rand_bytes(4))
1031283771
```

### 实现细节
Elixir在底层使用了Erlang对随机数的实现。`:rand.uniform/1` 返回1到N的随机数, `:rand.uniform/0` 返回0到1的浮点随机数。

## 参考来源
- Erlang `:rand` 模块文档: http://erlang.org/doc/man/rand.html
- Elixir Enum.random: https://hexdocs.pm/elixir/Enum.html#random/1
- Elixir 在编程的实践中生成随机数的讨论: https://elixirforum.com/t/how-do-you-generate-a-random-number-in-elixir/1983