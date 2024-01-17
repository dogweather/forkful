---
title:                "ランダムな数字の生成"
html_title:           "Ruby: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?

ランダムな数字を生成することは、プログラマーがランダムな入力を必要とする場合に使用されるテクニックです。これにより、ランダムな動作を再現したり、ユーザーにランダムなデータを提供したりすることができます。

## How to:

```Ruby
# 1から10までのランダムな数字を出力する方法
puts rand(1..10) #=> 7

# アルファベットからランダムに文字を出力する方法
puts ('a'..'z').to_a.sample(5).join  #=> "kwdos"
```

## Deep Dive

ランダムな数字の生成は、コンピュータサイエンスで非常に重要な役割を果たしてきました。1950年代にジョン・フォン・ノイマンが考案した、線形合同法というアルゴリズムは、多くのプログラミング言語で使用されています。しかし、このアルゴリズムは予測しやすいという欠点があり、より高度なアルゴリズムが開発されました。また、ハードウェアにおいても、ランダム性を得るために、温度や電流のノイズを利用したハードウェア乱数発生器が使用されます。

## See Also

- [SecureRandom クラス](https://docs.ruby-lang.org/en/3.0.0/SecureRandom.html)
- [線形合同法についての説明](https://ja.wikipedia.org/wiki/%E7%B7%9A%E5%BD%A2%E5%90%88%E5%90%8C%E6%B3%95)