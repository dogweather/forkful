---
title:    "C#: 文字列の大文字にする"
keywords: ["C#"]
---

{{< edit_this_page >}}

# なぜ文字列を大文字にするのか

プログラミングで文字列を扱うとき、時には大文字にする必要があります。この投稿では、C#で文字列を大文字にする方法について説明します。

## 方法

C#で文字列を大文字にする方法はいくつかあります。まずは、文字列を引数として渡す「ToUpper()」メソッドを使用する方法です。

```C#
// 文字列を定義する
string myString = "hello world";

// ToUpper()メソッドを使用して文字列を大文字にする
string uppercaseString = myString.ToUpper();

// 結果を出力する
Console.WriteLine(uppercaseString);

// 出力結果: HELLO WORLD
```

次に、C#の文字列リテラルで含まれる「!」演算子を使用する方法があります。

```C#
// 文字列を定義する
string myString = "hello world";

// !演算子を使用して文字列を大文字にする
myString = !myString;

// 結果を出力する
Console.WriteLine(myString);

// 出力結果: HELLO WORLD
```

## 深堀り

文字列を大文字にする方法には、上記のほかにもいくつかあります。一つは「char.ToUpper()」メソッドを使用する方法です。このメソッドは、文字列ではなく個々の文字を大文字にすることができます。

また、C#の「String.ToUpperInvariant()」メソッドを使用すると、英語以外の言語でも文字列を大文字にすることができます。

## 参考リンク

- [Microsoftドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.toupper?view=net-5.0)
- [C# 8.0の新機能：文字列リテラルを大文字にできるようにする](https://devenvexe.com/csharp-8-string-literals-uppercases/)
- [char.ToUpper()メソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.char.toupper?view=net-5.0)
- [String.ToUpperInvariant()メソッドのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant?view=net-5.0)