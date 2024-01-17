---
title:                "ランダムな数字の生成"
html_title:           "Elixir: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

"Elixirでランダムな数字を生成する方法"

## What & Why?
ランダムな数字を生成することは、プログラマにとって非常に重要です。これにより、偶然性や多様性を持ったデータを生成することができます。プログラマは、この機能を使用して、ランダムなアイテムを作成したり、ランダム性を持ったゲームをプレイできるようにしたりすることができます。

## How to:
Elixirでは、ランダム性を生成するために内蔵された```Enum.random/1```関数を使用することができます。この関数は、引数として渡されたリストからランダムな要素を返します。

```Elixir
numbers = [1, 2, 3, 4, 5]
Enum.random(numbers)
# Output: 3
```

また、特定の範囲内のランダムな整数を生成するには、```:rand.uniform/1```関数を使用することができます。例えば、1から10までの範囲内の整数を生成するには、以下のようになります。

```Elixir
:rand.uniform(1..10)
# Output: 7
```

## Deep Dive:
ランダム性を生成する方法は、プログラミング言語によって異なります。Elixirでは、Pseudorandom Number Generator（疑似乱数生成器）と呼ばれるアルゴリズムを使用してランダム性を生成します。疑似乱数生成器は、ある値（seed）をもとにして、予測できないようにランダムな値を生成します。

疑似乱数生成器以外にも、真のランダム性を生成する方法としてハードウェアデバイスを使用する方法もあります。Elixirでも、```:rand.uniform/2```関数の第二引数としてランダム性を生成する方法を指定することができます。

## See Also:
- <https://hexdocs.pm/elixir/1.11/Kernel.html#random/1>
- <https://elixirschool.com/ja/lessons/basics/control-structures/#%E3%83%A9%E3%83%B3%E3%83%80%E3%83%A0%E5%B0%8F%E6%95%B0>
- <https://www.tutorialspoint.com/elixir/elixir_random_number.htm>