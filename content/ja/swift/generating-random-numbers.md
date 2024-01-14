---
title:    "Swift: ランダムな数を生成する"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# なぜランダムな数字を生成するのか？

ランダムな数字を生成することには、さまざまな用途があります。たとえば、ゲームや宝くじなどのギャンブルで使用するためにランダムな数値が必要な場合や、ランダムなデータを使ってテストを行う場合などが挙げられます。

# ランダムな数字を生成する方法

Swiftでは、Int型の乱数を生成するために`arc4random_uniform()`メソッドが使用できます。このメソッドは、指定した範囲内のランダムな整数を返します。例えば、1から100までのランダムな数字を生成する場合は以下のように記述します。

```Swift
let randomNumber = Int(arc4random_uniform(100)) + 1
print(randomNumber)
```

このコードの出力例は、以下の通りです。

```
58
```

# ランダムな数字を生成する際の深堀り

乱数を生成するたびに同じ数字が出てしまう可能性があります。そのため、よりランダムな数を生成するには、現在時刻を使用することができます。

```Swift
let randomNumber = Int(Date().timeIntervalSince1970) % 100
print(randomNumber)
```

このコードでは、現在の時刻を元にランダムな数字が生成されます。もし、同じ時刻で生成された場合でも、`%`演算子によって100で割った余りが返されるので、より多様な数字が生成されるでしょう。

# See Also
- [Swift公式ドキュメント - arc4random_uniform()](https://developer.apple.com/documentation/swift/1558373-arc4random_uniform)
- [ランダムな数字を生成する方法 ー Qiita](https://qiita.com/KikurageChan/items/6f4ea8c0466d9ba097fd)
- [Swiftで乱数を生成する - Swiftサンプルコード集](https://swift-salaryman.com/random.php)