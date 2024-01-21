---
title:                "ランダム数の生成"
date:                  2024-01-20T17:50:02.961032-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダム数の生成とは、予測不可能な数を作ることです。シミュレーション、ゲーム、セキュリティで重要な役割を果たしています。

## How to: (方法)
Rubyでランダム数を生成する一番基本的な方法は、`rand` メソッドを使うことです。以下のコード例を参考にしてください。

```Ruby
# 0以上1未満の浮動小数点数を生成
random_float = rand
puts random_float # => 0.437598937072275

# 0から10までの整数を生成
random_int = rand(11)
puts random_int # => 5

# 範囲オブジェクトを使ってランダム数を生成
random_range = rand(1..6)
puts random_range # => 3
```

## Deep Dive (深掘り)
ランダム数を生成する概念はコンピューターサイエンスにおいて昔からある。実は、完全なランダム性を持つ数を生成することはできません。なぜなら、アルゴリズムに基づいて生成された数は「擬似ランダム数」と呼ばれます。Rubyの`Random` クラスでは、メルセンヌ・ツイスターというアルゴリズムを使っています。

他の方法として`SecureRandom` モジュールがあります。こちらはセキュリティの要求が高い場面で使われることが多いです。

```Ruby
require 'securerandom'

# より安全なランダムなバイトストリング生成
secure_random_bytes = SecureRandom.random_bytes(10)
puts secure_random_bytes.unpack1('H*') # => "f9168c5ebe49"

# 安全な16進数の文字列を生成
secure_hex = SecureRandom.hex(10)
puts secure_hex # => "83b031c3c5ed8e258f"
```

## See Also (関連情報)
- Rubyの公式ドキュメント: [Random class](https://docs.ruby-lang.org/en/3.1/Random.html), [SecureRandom module](https://docs.ruby-lang.org/en/3.1/SecureRandom.html)
- メルセンヌ・ツイスターに関する情報: [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)
- 乱数生成に関する一般的な情報: [Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)