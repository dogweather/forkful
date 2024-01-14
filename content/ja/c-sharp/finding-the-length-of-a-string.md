---
title:    "C#: 文字列の長さを見つける"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーたちは、文字列の長さを決定することがしばしばあります。例えば、入力フィールドの制限を設定したり、文字列を正しくフォーマットしたりするために必要です。今日は、文字列の長さを見つける方法について学びましょう！

##  方法

プログラミング言語C #では、文字列の長さを決定するためにLengthプロパティを使います。例えば、次のようなコードを書くことで、文字列の長さを決定することができます。

```C#
string input = "こんにちは、世界！"; 
int length = input.Length;
Console.WriteLine(length); // 出力： 8
```

上記のコードでは、input変数のLengthプロパティを使用して、文字列の長さを決定し、結果をlength変数に格納しています。その後、出力関数を使用して、結果をコンソールに表示しています。

## ディープダイブ

文字列の長さを決定する方法を深く掘り下げると、文字列の長さを決定するために内部でどのような処理が行われているかを知ることができます。C＃には、内部的に文字のバイト数をカウントする方法が実装されています。また、C＃では、Unicode文字を正しくサポートし、文字列の長さを決定するのに役立つ多くの関数が備わっています。

##  それを参照してください

- [C＃のString.Lengthプロパティのドキュメンテーション](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1) 
- [文字列の長さを決定するには？@Qiita（日本語）](https://qiita.com/shinoki7/items/3640d9bd1c91c1288001) 
- [The Truth About Strings in .NET@Pluralsight（英語）](https://www.pluralsight.com/courses/the-truth-about-strings-dotnet)