---
date: 2024-01-20 17:57:35.853150-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u6587\
  \u5B57\u5217\u5185\u3067\u7279\u5B9A\u306E\u6587\u5B57\u3084\u5358\u8A9E\u3092\u898B\
  \u3064\u3051\u3066\u3001\u4ED6\u306E\u3082\u306E\u3068\u4EA4\u63DB\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\
  \u6574\u5F62\u3001\u30B3\u30FC\u30C9\u4FEE\u6B63\u3001\u307E\u305F\u306F\u81EA\u52D5\
  \u5316\u30BF\u30B9\u30AF\u306E\u305F\u3081\u306B\u3088\u304F\u4F7F\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.123485-07:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u6587\
  \u5B57\u5217\u5185\u3067\u7279\u5B9A\u306E\u6587\u5B57\u3084\u5358\u8A9E\u3092\u898B\
  \u3064\u3051\u3066\u3001\u4ED6\u306E\u3082\u306E\u3068\u4EA4\u63DB\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\
  \u6574\u5F62\u3001\u30B3\u30FC\u30C9\u4FEE\u6B63\u3001\u307E\u305F\u306F\u81EA\u52D5\
  \u5316\u30BF\u30B9\u30AF\u306E\u305F\u3081\u306B\u3088\u304F\u4F7F\u3044\u307E\u3059\
  \u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
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
