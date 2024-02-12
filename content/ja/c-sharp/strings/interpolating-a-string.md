---
title:                "文字列の補間"
aliases:
- /ja/c-sharp/interpolating-a-string.md
date:                  2024-01-20T17:50:41.261255-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の補間"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (なぜ？とは？)
文字列補間とは、文字列の中に直接変数などを埋め込むことです。使う理由は簡単で、コードの読みやすさを保ちつつ、動的に文字列の内容を構築できるからです。

## How to: (方法)
```C#
string name = "Taro";
int age = 28;
string message = $"こんにちは、{name}さん。あなたは{age}歳ですね。";
Console.WriteLine(message);
```
出力:
```
こんにちは、Taroさん。あなたは28歳ですね。
```

## Deep Dive (探求)
昔は`String.Format()`や文字列の連結を利用していましたが、C# 6.0から文字列補間が導入され、コードが格段に読みやすくなりました。補間は`$`記号を使い、中括弧`{}`で変数を囲むだけ。内部的には`String.Format()`が使用されているので、パフォーマンス上の大きな違いはありません。ただし、コンパイラが生成するILコードは若干異なることがあるので面白いですね。他の方法としては、`StringBuilder`を使う方法もありますが、簡単に済ませるには文字列補間がベストです。

## See Also (関連情報)
- Microsoftの公式ドキュメント: [String interpolation (C# Reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
