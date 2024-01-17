---
title:                "文字列の長さを見つける"
html_title:           "C#: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なに & なぜ？

文字列の長さを見つけることは、プログラマーにとって重要なタスクの一つです。文字列の長さを知ることにより、実行時にメモリをどれくらい使うかや、文字列操作を行う方法を決めることができます。

## 方法：

```C#
string text = "こんにちは！";
int length = text.Length;
Console.WriteLine(length);
// 出力結果： 5
```

文字列オブジェクトの「Length」プロパティを使うことで、簡単に文字列の長さを取得することができます。上記の例では、"こんにちは！"という文字列の長さは5であることがわかります。

## ディープダイブ：

文字列の長さを見つけることは、昔からプログラミングにおいて重要な課題でした。古くは、メモリ使用量の最適化のために、プログラム内で使われる文字列の長さを事前に計算する方法が用いられていました。しかし、現代ではコンピューターの性能が向上し、このような最適化をする必要はあまりありません。また、他のプログラミング言語では「文字列」型と「文字」型が分かれているため、文字列の長さを見つける方法も異なる場合があります。

## 関連リンク：

- [C# ガイド: 文字列型](https://docs.microsoft.com/ja-jp/dotnet/csharp/language-reference/builtin-types/string)
- [C# ガイド: プロパティ](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/classes-and-structs/properties)