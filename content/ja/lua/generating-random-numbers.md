---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ?

ランダムな数値生成は、事実上制御不能な一連の数値を生成するプログラミングテクニックです。それはシミュレーション、ゲームの要素、暗号化において、予測不可能性を追加するのに役立ちます。

## どうやって:

以下、Luaでのランダム数値の生成について紹介します。

```Lua
math.randomseed(os.time())

random_number = math.random()
print(random_number)

random_number = math.random(100)
print(random_number)

random_number = math.random(50, 100)
print(random_number)
```

このコードは実行する度に、`(0,1)`間の浮動小数点数、`(1,100)`間の整数、そして`(50,100)`間の整数をそれぞれランダムに出力します。

## ディープダイブ:

1. 履歴: 計算機が最初に開発されたときから、開発者はランダム性を成し遂げるための方法を模索し続けてきました。初期の方法は、現在の時間やハードウェアノイズなど、予測できない要素を元にランダム数値を生成するものでした。

2. 代替手段: あるコンテキストでは擬似乱数生成器（PRNG）を使用することが有益です。これらは予測可能で反復可能な「ランダム」数列を生成します。Luaでは、ユーザーは標準ライブラリの`math.random`関数、または外部ライブラリを使用してPRNGを実装できます。

3. 実装の詳細: Luaの`math.random`関数は、Cの標準ライブラリ関数`rand`を使用しています。なお、最適な乱数を生成するために、Luaでは`math.randomseed(os.time())`で乱数生成器に種(seed)を設定します。

## 関連する情報:

- Luaの公式ドキュメンテーション: [math library](https://www.lua.org/manual/5.4/manual.html#6.7)
- PRNGについての詳細: [Wikipedia](https://ja.wikipedia.org/wiki/擬似乱数)
- ランダム数値生成についての深い理解: [Random.org](https://www.random.org/randomness/)