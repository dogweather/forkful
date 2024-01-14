---
title:    "C#: テキストの検索と置換"
keywords: ["C#"]
---

{{< edit_this_page >}}

## なぜ
テキストを検索して置換することの利点は、複数の文書やファイル内の特定のテキストを自動的に変更できることです。これにより、手作業での大量のテキスト変更を行う必要がなくなり、時間を節約することができます。

## 使い方
以下のC#コードの例を参考にして、テキストの検索と置換をどのように行うかを学びましょう。
```C#
// 文字列内での置換
string text = "こんにちは、世界！";
string newText = text.Replace("世界", "地球");
Console.WriteLine(newText);
// 出力: こんにちは、地球！

// ファイル内での置換
string filePath = @"C:\Documents\file.txt";
string fileContent = File.ReadAllText(filePath);
string newContent = fileContent.Replace("旧テキスト", "新しいテキスト");
File.WriteAllText(filePath, newContent);
```

## 深堀り
テキストの検索と置換の実行は、プログラミング言語によってさまざまな方法で行うことができます。C#では、.NETフレームワークの一部である`String.Replace()`メソッドを使用することで簡単に置換を行うことができます。また、`File.ReadAllText()`や`File.WriteAllText()`メソッドを使用することで、ファイル内のテキストを読み取ったり書き込んだりすることができます。さらに、正規表現を使用することでより高度な置換が可能になります。

## 関連リンク
- [C# String.Replace()メソッドのドキュメンテーション](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.replace?view=netcore-3.1)
- [C# Fileクラスのドキュメンテーション](https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file?view=netcore-3.1)
- [C# 正規表現の使用方法について学ぶ](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference#.NET_を使用した正規表現の一般的な使用方法)