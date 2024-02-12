---
title:                "テキストの検索と置換"
aliases: - /ja/c-sharp/searching-and-replacing-text.md
date:                  2024-01-20T17:57:35.853150-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
テキストの検索と置換は文字列内で特定の文字や単語を見つけて、他のものと交換することです。プログラマーはデータ整形、コード修正、または自動化タスクのためによく使います。

## How to: (やり方)
```C#
using System;

class Program {
    static void Main() {
        string originalText = "Hello World! Programming is fun.";
        string searchText = "World";
        string replaceText = "C# World";
        
        string newText = originalText.Replace(searchText, replaceText);
        
        Console.WriteLine(newText); // Output: Hello C# World! Programming is fun.
    }
}
```

## Deep Dive (深い潜入)
検索と置換の機能はものすごく古い。最初のテキストエディターから存在し、多くのプログラミング言語が独自の実装を持っています。

C#において、`String.Replace` メソッドは最も簡単な形。正規表現を使う `Regex.Replace` はもっと複雑なパターンに強い。また、LINQを使って独自の置換ロジックを構築することもできる。

実装の詳細については、パフォーマンス（大きなテキストでは `StringBuilder` を考慮）とセキュリティ（ユーザー入力を扱う場合のサニタイズ）も忘れずに。

## See Also (参照)
- [String.Replace メソッドの公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.replace)
- [正規表現との連携](https://docs.microsoft.com/ja-jp/dotnet/api/system.text.regularexpressions.regex.replace)
- [LINQの公式ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/concepts/linq/)
