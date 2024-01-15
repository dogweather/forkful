---
title:                "「文字列の長さを求める」"
html_title:           "C#: 「文字列の長さを求める」"
simple_title:         "「文字列の長さを求める」"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ
*なぜ* 文字列の長さを求めることについて取り組むのかについて、最大２文で説明します。

文字列の長さを知ることは、プログラミングにおいて非常に重要なスキルです。例えば、入力された文字列が指定された長さと一致するかどうかを確認する際に使用されます。また、文字列を扱う上でのデータ処理におけるメソッドの引数としても使用されます。

## How To

文字列の長さを求めるためには、```Length``` プロパティを使用します。以下の例を参考にしてください。

```C#
string food = "Sushi";
Console.WriteLine(food.Length); //Output: 5
```

また、文字列の長さを求める際には、空白スペースや記号も文字数としてカウントされることに注意してください。例えば、以下の例ではスペースを含めた10文字がカウントされます。

```C#
string sentence = "I love coding!";
Console.WriteLine(sentence.Length); //Output: 10
```

## Deep Dive

```Length``` プロパティは、```String``` クラスのメソッドとして定義されています。このプロパティは、文字列の内部的な文字配列の長さを返します。そのため、文字列の長さを求める際には、文字列を反復処理する必要はありません。

また、C#では ```Length``` プロパティのほかにも、文字列の長さを求めるための様々なメソッドが提供されています。例えば、```Count()``` メソッドを使用することで、指定した文字の数をカウントすることができます。

## See Also

- [String.Length Property (C# Reference)](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [String.Count Method (System.Linq) ](https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.count?view=net-5.0)