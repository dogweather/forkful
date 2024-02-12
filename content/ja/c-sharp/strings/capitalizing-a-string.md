---
title:                "文字列を大文字にする"
aliases: - /ja/c-sharp/capitalizing-a-string.md
date:                  2024-02-03T19:05:25.678937-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
C#で文字列の最初の文字を大文字にすることは、その文字が既に大文字でない場合に、最初の文字を大文字に変換することを含みます。この変更は、出力のフォーマット、コーディング標準の適用、またはユーザーインターフェーステキストの読みやすさを向上させるために、重要な場合があります。

## どのように：
C#は、組み込みのメソッドを使用して文字列を大文字化する直接的な方法を提供します。これを達成する最も簡単な方法は、これらのメソッドで文字列を直接修正することです。より複雑または特定の大文字化ルール（例えば、各単語の最初の文字を大文字にする）については、追加のライブラリや手動の方法が必要になるかもしれません。以下に、C#で様々な方法で文字列を大文字化する方法を示す例を紹介します。

### 基本的な大文字化：
単語または文の最初の文字を大文字にするには：

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // 出力: "Hello world"
```

### 各単語の最初の文字を大文字にする：
文字列の各単語の最初の文字を大文字にするためには、`System.Globalization`名前空間にある`TextInfo.ToTitleCase`メソッドを使用できます：

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // 出力: "Hello World"
```

注意：`ToTitleCase`は残りの文字のケースを小文字には変えず、単語の最初の文字のみを大文字に変更します。また、タイトルケースルールの特定の単語（「and」、「or」、「of」など）は、文化設定によって大文字にされない場合があります。

### 再利用性のための拡張メソッドの使用：
大文字化プロセスを簡素化するために`string`クラスの拡張メソッドを作成し、コードをよりクリーンで再利用可能にすることができます。以下は、そのようなメソッドを作成して使用する方法です：

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // 出力: "Hello world"
    }
}
```

この拡張メソッド`Capitalize`は、名前空間内の任意の文字列オブジェクトで呼び出すことができ、C#での文字列操作をより直感的でオブジェクト指向的なアプローチを提供します。

### サードパーティライブラリの使用:
C#の標準ライブラリは、文字列の大文字化や、文字列の各単語を大文字化するためのほとんどのニーズをカバーしている一方で、特定の特化したタスクには、Humanizerのようなサードパーティライブラリが有益な場合があります。しかし、単に文字列や各単語を大文字化するタスクのためには、標準のC#メソッドが十分で効率的であり、外部依存性の必要性を否定します。
