---
title:                "テキストの検索と置き換え"
html_title:           "C#: テキストの検索と置き換え"
simple_title:         "テキストの検索と置き換え"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
なぜ、テキストの検索と置換に取り組む必要があるのでしょうか？プログラミングにおいて、コード内の一部を変更する際に効率的かつ手軽に行うことができるからです。例えば、大規模なプロジェクトで同じセクションの変数名を変更したい場合、手作業で全てを修正するよりも、検索と置換を使う方がはるかに早く確実です。

## How To
`Regex.Replace()`メソッドを使用して、C#プログラムでテキストの検索と置換を行うことができます。下記の例は、文字列から数字のみを抽出するプログラムです。

```C#
using System;
using System.Text.RegularExpressions;

namespace SearchAndReplaceExample
{
    class Program
    {
        static void Main(string[] args)
        {
            string text = "This is a 1234 sample text with 5678 numbers.";
            string pattern = "[^0-9]"; // 正規表現：0から9以外の文字
            string result = Regex.Replace(text, pattern, ""); // 数字以外を空文字に置換

            Console.WriteLine(result);
        }
    }
}

// Output:
// 12345678
```

上記のコードでは、正規表現のパターンを使用して、数字以外を空文字に置換しています。もちろん、この正規表現を変更することで、様々な検索と置換のパターンを作成することができます。

## Deep Dive
上記の例では、単純な検索と置換を紹介しましたが、実際にはもっと複雑なパターンを作成することができます。`Regex`クラスには、様々な検索や置換を行うための便利なメソッドが用意されています。また、正規表現のパターンをより高度に作成するための一般的なルールやコツもあります。より詳細な情報を知りたい場合は、MSDNやオンラインのコミュニティで質問することをお勧めします。

## See Also
参考になる記事や資料を紹介します：
- [C# の正規表現をマスターしよう！](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [正規表現のパターン - C# リファレンス](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/regular-expression-patterns)
- [正規表現のトリック集](https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial)