---
title:                "C#: 文字列の長さを求める"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることの意義は、プログラミングでテキストを処理する際に非常に重要です。例えば、ユーザーに入力された文字列をチェックするときや、ファイルの内容を扱うときに、文字列の長さを知る必要があります。

## 方法

```C#
// 文字列の長さを求める方法
string text = "こんにちは、世界！";
int length = text.Length;
Console.WriteLine(length); // 出力結果: 9
```

上記のコードでは、`string`型の変数`text`に文字列を代入し、`Length`プロパティを使用して文字列の長さを取得しています。このプロパティは、文字列の全ての文字をカウントしてくれるので、日本語や英語、数字などの多言語にも対応しています。また、空白や句読点も文字としてカウントされるので注意が必要です。

## 深堀り

文字列の長さを求める方法には、`Length`プロパティ以外にも`Count`メソッドを使用する方法もあります。しかし、`Count`メソッドは文字列の長さを求めるだけでなく、指定した条件に合致した文字の数をカウントすることもできます。これは、特定の文字を探して処理を行う場合などに便利です。

## よくある質問

- 日本語の文字列の長さは、英文字列の長さと同じですか？
  - はい、日本語の文字列も全ての文字をカウントして長さを判断します。
- 文字列の先頭や末尾にある空白や改行は、文字列の長さに含まれるのですか？
  - はい、文字列の先頭や末尾の空白や改行も文字としてカウントされます。

## 参考リンク

- [.NET プログラミング ガイド: 文字列の基本](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/strings)
- [C# マニュアル: string.Length プロパティ](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.length)