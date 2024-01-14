---
title:                "C#: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# *なぜ*文字列を小文字に変換するのか

文字列の大文字と小文字は、プログラミングにおいて非常に重要です。例えば、ユーザーが入力した文字列をデータベースに保存する場合、正しい検索が行われるように大文字と小文字を区別する必要があります。また、文字列を比較する場合にも、大文字と小文字の差が重要になってきます。そのため、文字列を処理する際には、必要に応じて大文字と小文字を変換する必要があります。

## *方法*

文字列を小文字に変換するには、C#においては `ToLower()` メソッドを使用することができます。下記のコード例を参考にしてください：

```C#
string name = "JOHN DOE";
string lowercaseName = name.ToLower();

Console.WriteLine(lowercaseName);
```

上記のコードでは、変数 `name` には大文字の "JOHN DOE" が格納されており、それを `ToLower()` メソッドを使用して小文字に変換し、`lowercaseName` 変数に格納しています。最後に `Console.WriteLine()` メソッドを使用して、変換された文字列を表示しています。

実行結果：

```
john doe
```

## *深堀り*

文字列を小文字に変換する際には、ASCII文字セットとユニコード文字セットの違いに注意する必要があります。ASCII文字セットでは、大文字と小文字の間には厳密な関係がありますが、ユニコード文字セットでは、そのような関係がありません。そのため、`ToLower()` メソッドを使用する場合は、必ずどの文字セットを対象に変換を行うのかを考慮しなければなりません。

また、`ToLower()` メソッドは、文字列オブジェクトが存在する場合、そのオブジェクトを変更することなく新しい文字列オブジェクトを返します。そのため、変換前の文字列オブジェクトには影響を与えることなく、新しい変換後の文字列オブジェクトを使用することができます。

## *参考リンク*

- [C# string.ToLower() メソッドのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [ASCII文字セットとユニコード文字セットとの違いについての説明](https://www.japanesestation.com/ascii/)
- [C# string.ToLower() メソッドを使った文字列の変換の例](https://www.tutorialspoint.com/csharp/csharp_string_tolower.htm)

# 参考リンク