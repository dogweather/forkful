---
date: 2024-01-20 17:57:35.853150-07:00
description: "How to: (\u3084\u308A\u65B9) \u691C\u7D22\u3068\u7F6E\u63DB\u306E\u6A5F\
  \u80FD\u306F\u3082\u306E\u3059\u3054\u304F\u53E4\u3044\u3002\u6700\u521D\u306E\u30C6\
  \u30AD\u30B9\u30C8\u30A8\u30C7\u30A3\u30BF\u30FC\u304B\u3089\u5B58\u5728\u3057\u3001\
  \u591A\u304F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u304C\u72EC\
  \u81EA\u306E\u5B9F\u88C5\u3092\u6301\u3063\u3066\u3044\u307E\u3059\u3002 C#\u306B\
  \u304A\u3044\u3066\u3001`String.Replace` \u30E1\u30BD\u30C3\u30C9\u306F\u6700\u3082\
  \u7C21\u5358\u306A\u5F62\u3002\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u3046 `Regex.Replace`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.980822-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u691C\u7D22\u3068\u7F6E\u63DB\u306E\u6A5F\u80FD\u306F\
  \u3082\u306E\u3059\u3054\u304F\u53E4\u3044\u3002\u6700\u521D\u306E\u30C6\u30AD\u30B9\
  \u30C8\u30A8\u30C7\u30A3\u30BF\u30FC\u304B\u3089\u5B58\u5728\u3057\u3001\u591A\u304F\
  \u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u304C\u72EC\u81EA\u306E\
  \u5B9F\u88C5\u3092\u6301\u3063\u3066\u3044\u307E\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

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
