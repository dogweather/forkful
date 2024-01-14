---
title:                "Elixir: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

### 簡単に生成できる - ランダム数字の魅力

乱数を生成することは、エラプログラミングにおいて非常に重要です。不用意に作成してしまうことで、大きな問題を引き起こすことがあります。しかし、正しく扱うことができれば、乱数は非常に便利で有用なものです。その魅力について、今日は紹介します。

## Why: なぜ乱数を生成するのか

乱数を生成することで、様々な用途に役立てることができます。例えば、ゲームでの乱数生成は不可欠です。また、データのサンプリングやシャッフルなどにも乱数は活用されます。さらに、暗号学やセキュリティにおいても乱数は重要な役割を果たします。

## How To: 実際にコードを書いてみよう

```Elixir
# 0以上9以下のランダムな整数を生成する
rand(0..9) # => 5

# 0以上10未満のランダムな浮動小数点数を生成する
rand(0..10) # => 8.2345

# リストの中からランダムな要素を選択する
list = [1, 2, 3, 4, 5]
list |> Enum.at(rand(0..4)) # => 3

# Elixirのモジュールで定義された関数を使用する
:random.seed(50)
rand_uniform() # => 0.7542397

# Elixir 1.8以上では、SecureRandomモジュールを使用することで暗号学的に安全な乱数を生成することができます
SecureRandom.hex(16) # => "8bf37817e53c5e50bef1eed3675dc59b"

```

## Deep Dive: 乱数生成についてより詳しく

乱数生成アルゴリズムは、いくつかの種類が存在します。例えば、線形合同法やメルセンヌ・ツイスター法などがあります。これらのアルゴリズムは計算機の種々の性能や特性、そして目的に合わせて洗練されています。

また、乱数生成は成功しないこともあります。例えば、同じシード値を使用した場合や、乱数生成器が永続的でない場合には、同じ値が生成される可能性があります。そのため、このような問題を避けるためには、十分なシード値やセキュリティ強度を持つ乱数生成器を使用することが重要です。

## See Also: 他にも参考になる情報を

- [Elixir 公式ドキュメント | ランダムモジュール](https://hexdocs.pm/elixir/Random.html)
- [CSDNブログ | Elixirで乱数を生成する](https://blog.csdn.net/qq_43159640/article/details/100189844)
- [Learn Elixir | 乱数生成器についてさらに学ぶ](https://learn-elixir.dev/rand-generating#seeded-random-number-generation)