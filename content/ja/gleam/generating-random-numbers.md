---
title:                "ランダムな数字を生成する"
html_title:           "Gleam: ランダムな数字を生成する"
simple_title:         "ランダムな数字を生成する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

##なぜ
ランダムな数字を生成することに興味があるのは、データの偏りを避け、選択肢をランダムにすることでより公平な意思決定を行うためです。

##作り方
ランダムな数字を生成するためには、Gleamの`rand`モジュールを使用します。まずは`Main`モジュールに以下のように記述します。

```Gleam
import rand

fn main() {
  // コードをここに書く
}
```

###一桁のランダムな整数を生成する
Gleamでは、`rand.int`関数を使用して指定された範囲内の一桁のランダム整数を生成することができます。

```Gleam
let number = rand.int(0, 9) // 範囲は0から9まで
```

###複数のランダムな整数を生成する
もし複数のランダムな整数を生成したい場合は、以下のように`for`ループを使用することができます。

```Gleam
let numbers = for _ in 1..5 {
  rand.int(10, 20) // 10から20までの整数を5つ生成
}

// numbers = [12, 16, 17, 10, 20]
```

###ランダムな文字列を生成する
文字列をランダムに生成するには、`rand.string`関数を使用します。

```Gleam
let letters = "abcdefghijklmnopqrstuvwxyz"
let string = rand.string(letters, 10) // 10文字のランダムな文字列を生成

// string = "jgixskdelt"
```

##深く掘り下げる
Gleamの`rand`モジュールは内部で、Mersenne Twisterと呼ばれるアルゴリズムを使用してランダムな数字を生成します。このアルゴリズムは周期が2^19937-1であり、非常に高速かつランダムな数字を生成することができます。

##参考情報
- [Gleamのrandモジュール](https://gleam.run/modules/rand)
- [Mersenne Twisterアルゴリズムの詳細](https://en.wikipedia.org/wiki/Mersenne_Twister)