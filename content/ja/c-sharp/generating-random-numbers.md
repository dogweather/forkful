---
title:                "乱数の生成"
html_title:           "C#: 乱数の生成"
simple_title:         "乱数の生成"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することの利点はたくさんあります。ゲームやシミュレーション、暗号学などのさまざまな分野で使用されます。 

## 使い方

```C#
// 1から10までのランダムな整数を生成する
int num = new Random().Next(1, 11);
Console.WriteLine(num);
// Output: 7

// 0から1未満のランダムな小数を生成する
double decimal = new Random().NextDouble();
Console.WriteLine(decimal);
// Output: 0.562914183

// ランダムな文字列を生成する
// ASCII文字の範囲内から10文字をランダムに選択して生成する
char[] chars = "abcdefghijklmnopqrstuvwxyz0123456789".ToCharArray();
Random random = new Random();
string str = "";
for (int i = 0; i < 10; i++)
{
    int index = random.Next(0, chars.Length);
    str += chars[index];
}
Console.WriteLine(str);
// Output: rcv80wup7z

// ランダムな整数の配列を生成する
int[] nums = new int[5];
Random rand = new Random();
for (int i = 0; i < nums.Length; i++)
{
    nums[i] = rand.Next(1, 101);
}
Console.WriteLine(string.Join(", ", nums));
// Output: 64, 32, 91, 10, 53
```

## 詳しく見る

ランダムな数字を生成する方法はさまざまありますが、その生成方法にはいくつかの種類があります。C#では、`System.Random`クラスを使用してランダムな数字を生成することができます。多くの場合、`Next()`メソッドを使用して指定した範囲内のランダムな整数を生成しますが、小数や文字列を生成することも可能です。また、`Random`クラスには双方向性を持つ`NextBytes()`メソッドがあるため、暗号学的用途にも使用することができます。 

## 関連リンク

- [C#のランダムな数字の生成方法のドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.random)
- [C#のRandomクラスの公式チュートリアル](https://docs.microsoft.com/ja-jp/dotnet/core/tutorials/using-with-xplat-cli)
- [C#で文字列をランダムに生成する方法](https://stackoverflow.com/questions/1344221/how-can-i-generate-random-alphanumeric-strings)