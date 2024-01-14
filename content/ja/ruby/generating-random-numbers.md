---
title:                "Ruby: ランダムな数を生成する"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

##なぜ

ランダムな数字を生成することの価値は、多くの場面で活用することができるためです。例えば、ゲームやシミュレーション、データのサンプリングやテストなど、多くのプログラミングの分野でランダムな数字が必要となります。また、乱数を生成することで、プログラムの様々な機能をテストすることも可能です。

##作り方

以下のコードブロック内に、Rubyを用いてランダムな数字を生成する方法を示します。

```Ruby
# ランダムな整数を1から10までの範囲で生成する
rand(1..10)

# ランダムな小数を0から1までの範囲で生成する
rand

# ランダムな文字列を生成する
("a".."z").to_a.shuffle.join
```

上記のコードを実行すると、例えば以下のような出力が得られます。

```Ruby
7
0.546294834
voedpgsjihcfmwxqyrzatlubnk
```

##深堀り

乱数生成に用いられるアルゴリズムには、線形合同法やメルセンヌ・ツイスター法などがあります。これらは、初期値やルールによって擬似乱数を生成するものであり、完全なランダムではないことに注意が必要です。

また、Rubyの`rand`メソッドは、乱数のシードを指定しない場合、プログラムの実行時刻をシードとして使用します。そのため、同じプログラムを実行しても異なる結果が得られることがあります。このような性質を持つため、シードを指定すれば再現性のある乱数を生成することも可能です。

##関連リンク

- [Rubyで乱数を生成する方法 (Qiita)](https://qiita.com/airtoxin/items/71b2dfe14c68b45b5a08)
- [乱数の生成 (Rubyドキュメント)](https://docs.ruby-lang.org/ja/latest/method/Kernel/m/rand.html)
- [線形合同法 (Wikipedia)](https://ja.wikipedia.org/wiki/%E7%B7%9A%E5%BD%A2%E5%90%88%E5%90%8C%E6%B3%95)