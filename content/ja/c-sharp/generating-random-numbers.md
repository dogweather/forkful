---
title:                "C#: 生成するランダムな数"
simple_title:         "生成するランダムな数"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜランダムな数字を生成するのか

ランダムな数字を生成することは、さまざまなソリューションやアルゴリズムを作成する際に必要です。例えば、ゲーム開発や暗号学においてランダムな要素を取り入れたい場合に使用されます。ランダムな数字を生成することで、より多様なシナリオやパターンを作り出すことができます。

## 作り方

ランダムな数字を生成するには、C#プログラミング言語の標準ライブラリである"System.Random"クラスを使用します。

```
using System;

class Program
{
    static void Main()
    {
		// ランダムな数字を生成するためのSystem.Randomクラスのインスタンスを作成する
        Random rand = new Random();

        // 1から10までの範囲からランダムな数字を生成する
        int number = rand.Next(1, 11);

        Console.WriteLine("ランダムな数字: " + number);
    }
}
```

上記のコードを実行すると、1から10までの範囲からランダムな数字がコンソールに表示されます。毎回実行するたびに、異なる数字が表示されることを確認しましょう。

## ディープダイブ

System.RandomクラスのNext()メソッドは、引数として最小値と最大値を指定することで、その範囲内からランダムな整数を生成します。また、NextDouble()メソッドを使用することで、0以上1未満の範囲からランダムな小数を生成することも可能です。

さらに、ランダムな乱数列を生成するにはSeed値を指定する必要があります。Seed値は、ランダムな数字を生成するアルゴリズムに使用される初期値であり、同じSeed値を指定することで同じ乱数列が生成されます。

以上のように、ランダムな数字を生成する方法はさまざまありますが、そのすべてが正確な乱数列を生成するとは限りません。厳密な暗号学的な用途では、専門的な乱数生成器を使用することをお勧めします。

## 関連リンク

- [Microsoft Docs - Random Class (C#)](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- [C# リファレンス - System.Random クラス](https://docs.microsoft.com/ja-jp/dotnet/api/system.random)
- [C# ランダムな数字を生成する方法](https://www.tutunon.com/csharp-random-number-generator/)