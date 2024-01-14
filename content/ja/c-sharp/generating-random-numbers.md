---
title:                "C#: 「ランダム数値の生成」"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# なぜ乱数を生成する必要があるのか？

プログラミングで乱数を生成することは、ゲームやランダムなデータを必要とするアプリケーションを作成する際に非常に役立ちます。乱数は、プログラムにおける予期せぬバリエーションを作成し、より面白いものに変えることができるのです。

## 生成方法

乱数を生成するためには、C#言語におけるRandomクラスを使用します。まず、Randomクラスのインスタンスを作成し、その後、Nextメソッドを使って乱数を得ることができます。

```C#
// Randomクラスのインスタンスを作成
Random random = new Random();

// 0から10の間の乱数を生成
int randomNumber = random.Next(0, 10);

// 結果の出力
Console.WriteLine("ランダムな数字は：" + randomNumber);
```

上記のコードの場合、生成される乱数は毎回異なる値が得られます。また、同じ値が重複する可能性もあります。

## 詳細を探る

Randomクラスは、内部的に現在の時刻をシード値として使用しているため、毎回異なる乱数が得られるのです。また、RandomクラスにはさまざまなオーバーロードされたNextメソッドがあり、引数を変えることでさまざまな範囲の値を生成することができます。さらに、乱数を生成する前にシード値を指定することも可能です。

# このようにして乱数を生成することで、もっと楽しいプログラミング体験をすることができます。ぜひ試してみてください！

## さらに読む

- [C#のRandomクラスのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.random?view=netcore-3.1)
- [乱数生成についての詳しい解説](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E7%94%9F%E6%88%90)