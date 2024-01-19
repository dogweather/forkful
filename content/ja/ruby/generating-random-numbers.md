---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？
ランダムな数字の生成は、不確定性をコードに追加することです。これは、ゲームの結果を不確定にしたり、データの秘匿化に役立つなど、プログラムにバラエティを提供するためにプログラマーによって行われます。

## どうやって：
Rubyでは `rand` メソッドを使用してランダムな数を生成します。 ここにいくつかの例があります:

```Ruby
# ０と１の間のランダムな浮動小数点数を生成します
puts rand()

# 0～10の間のランダムな整数を生成します
puts rand(11) 

# 1～10の間のランダムな整数を生成します
puts rand(1..10)
```

ランダムな数値を生成すると次のように表示されます:
```Ruby
0.8975989316511212 
6
3
```

## 深いダイブ
ランダムな数字の生成は古くから存在し、コンピュータサイエンスの初期から主要な役割を果たしてきました。伪乱数(generator)と真の乱数(generator)の間の厳密な識別は、アプリケーションの要件により異なります。

Rubyの `rand` メソッドは、メルセンヌ・ツイスタというアルゴリズムを使用しています。これは、サイクルの長さが非常に長く、高速であるため、多くのアプリケーションで使用されます。

代替手段として、セキュアランダムライブラリはより安全なランダム数値を提供します。これは、暗号の世界での使用を見越しています。

```Ruby
require 'securerandom'

# Get a random number in a range (0 to 100)
puts SecureRandom.random_number(100) 
```

## 参考情報
Rubyの `rand` と `securerandom` メソッドについてさらに学びたい方は、以下のリンクをご参照ください。

- Ruby `rand` ：
  - [https://docs.ruby-lang.org/en/3.0.0/Kernel.html#method-i-rand](https://docs.ruby-lang.org/en/3.0.0/Kernel.html#method-i-rand)
- SecureRandom：
  - [https://docs.ruby-lang.org/en/2.5.0/SecureRandom.html](https://docs.ruby-lang.org/en/2.5.0/SecureRandom.html)