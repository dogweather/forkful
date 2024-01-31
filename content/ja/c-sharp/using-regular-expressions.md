---
title:                "正規表現の使用"
date:                  2024-01-19
simple_title:         "正規表現の使用"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
正規表現は文字列を探索、置換、解析するパターンです。プログラマーはコードを簡潔にし、複雑な文字列処理を効率よく行うために使用します。

## How to: (方法)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // 文字列中の電子メールアドレスを検索。
        string text = "連絡先: tanaka@example.com, yamada@example.net";
        string pattern = @"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b";
        
        MatchCollection matches = Regex.Matches(text, pattern);
        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```
サンプル出力：
```
tanaka@example.com
yamada@example.net
```

## Deep Dive (詳細情報)
- **歴史的背景**: 正規表現は1950年代に登場し、Unixのツールに実装され広まりました。
- **代替方法**: 文字列操作メソッド(`IndexOf`, `Substring`等)やパーサーがありますが、正規表現のように汎用性は低いです。
- **実装の詳細**: .NETの`System.Text.RegularExpressions`名前空間がC#での正規表現を提供します。パターンマッチングは背後で効率的なアルゴリズムを使用しています。

## See Also (関連情報)
- [Microsoft Regular Expressions Documentation](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Regular-Expressions.info](http://www.regular-expressions.info/)
- [Regex101: Online Regex Tester and Debugger](https://regex101.com/)
