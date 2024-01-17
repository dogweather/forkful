---
title:                "文字列の書き換え"
html_title:           "C#: 文字列の書き換え"
simple_title:         "文字列の書き換え"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何か: 文字列の大文字化とは何かを1〜2文で説明する。そしてプログラマーがそれをする理由を説明する。

プログラマーとして、私たちはさまざまな方法で文字列を処理します。たとえば、文字列を結合したり、切り抜いたり、置換したりすることができます。しかし、文字列を大文字化することもできます。これは単純に文字列をすべて大文字に変換することを意味します。プログラマーがこれをする理由の1つは、ソートや検索などの処理を簡単にするためです。

## 方法: C#のコードブロック ```C# ... ``` 内に、コーディングの例とサンプル出力を示す。

```
string hello = "こんにちは、世界！";
string capitalized = hello.ToUpper();

Console.WriteLine(capitalized);
// 出力：こんにちは、世界！
```

大文字化では、元の文字列を変数に格納し、文字列の `ToUpper()` メソッドを使用して大文字に変換します。これで、変換した文字列を新しい変数に割り当てることができます。

```
string greeting = "hello world";
string capitalized = greeting.ToUpper();

Console.WriteLine(capitalized);
// 出力：HELLO WORLD
```

## ディープダイブ: (1) 歴史的な文脈、(2) 代替手段、(3) 文字列の大文字化の実装の詳細などの情報を提供する。

文字列を大文字化するための一般的な方法は、文字列の `ToUpper()` メソッドを使用することです。しかし、これはすべての言語で利用できるわけではありません。例えば、古いC言語では、大文字化するために独自の関数を作成する必要がありました。さらに、一部のプログラミング言語では、大文字化と小文字化を区別するように設計されています。

## 関連リンク: 関連するソースへのリンクを提供する。

- [C# の ToUpper() メソッドのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [C# の文字列操作に関するチュートリアル](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [C# の文字列操作に関する記事](https://www.c-sharpcorner.com/article/strings-and-stringformat-in-c-sharp/)