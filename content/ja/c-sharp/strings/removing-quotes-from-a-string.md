---
date: 2024-01-26 03:38:53.176588-07:00
description: ''
lastmod: '2024-04-05T21:59:54.407998-06:00'
model: gpt-4-0125-preview
summary: "\u30AF\u30A9\u30FC\u30C8\u3092\u53D6\u308A\u9664\u304F\u6982\u5FF5\u306F\
  \u65B0\u3057\u3044\u3082\u306E\u3067\u3082\u7279\u306B\u8907\u96D1\u306A\u3082\u306E\
  \u3067\u3082\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u30AF\u30A9\u30FC\u30C8\u304C\
  \u3057\u3070\u3057\u3070\u6587\u5B57\u5217\u3092\u533A\u5207\u308B\u305F\u3081\u306B\
  \u4F7F\u7528\u3055\u308C\u308B\u305F\u3081\u3001\u91CD\u8981\u3067\u3059\u3002\u30A8\
  \u30B9\u30B1\u30FC\u30D7\u3055\u308C\u3066\u3044\u306A\u3044\u30AF\u30A9\u30FC\u30C8\
  \u304C\u30B3\u30FC\u30C9\u30D6\u30ED\u30C3\u30AF\u307E\u305F\u306F\u30C7\u30FC\u30BF\
  \u30D5\u30A1\u30A4\u30EB\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\u5217\u306B\u306F\
  \u3001\u6587\u5B57\u5217\u304C\u65E9\u671F\u306B\u7D42\u4E86\u3057\u3066\u3057\u307E\
  \u3044\u3001\u30A8\u30E9\u30FC\u3084\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u554F\u984C\
  \uFF08\u4F8B\u3048\u3070\u30A4\u30F3\u30B8\u30A7\u30AF\u30B7\u30E7\u30F3\u653B\u6483\
  \uFF09\u3092\u5F15\u304D\u8D77\u3053\u3059\u53EF\u80FD\u6027\u304C\u3042\u308A\u307E\
  \u3059."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法:
```csharp
string withQuotes = "\"Hello, World!\"";
Console.WriteLine($"Original: {withQuotes}");

// 二重引用符を削除
string withoutDoubleQuotes = withQuotes.Replace("\"", "");
Console.WriteLine($"Without Double Quotes: {withoutDoubleQuotes}");

// 単一引用符を削除（もともと文字列に含まれていた場合）
string withSingleQuotes = "'Hello, World!'";
string withoutSingleQuotes = withSingleQuotes.Replace("'", "");
Console.WriteLine($"Without Single Quotes: {withoutSingleQuotes}");
```

出力:
```
Original: "Hello, World!"
Without Double Quotes: Hello, World!
Without Single Quotes: Hello, World!
```

## 徹底解析
クォートを取り除く概念は新しいものでも特に複雑なものでもありませんが、クォートがしばしば文字列を区切るために使用されるため、重要です。エスケープされていないクォートがコードブロックまたはデータファイルに含まれる文字列には、文字列が早期に終了してしまい、エラーやセキュリティ問題（例えばインジェクション攻撃）を引き起こす可能性があります。

歴史的に、クォートを扱うことは、データ処理における検証やサニタイズのプロセスの一部でした。`.Replace()` メソッドは簡単な文字列からクォートを取り除くために直感的ですが、入れ子になったクォートや条件付き削除など、より複雑なシナリオを扱う場合には、正規表現のようなより高度な技術が必要になることがあります。

`.Replace()` の代わりとなる方法には、固定文字ではなくパターンを扱う際や細かな制御が必要なときに、`Regex` クラスからのメソッドがあります。たとえば、エスケープされた文字を扱う際には `Regex.Unescape()` が役立ちます。

実装上の注意点として、C#の文字列はイミュータブル（不変）であるため、`.Replace()` を使用するたびに新しい文字列が作成されます。これは、小規模または一度きりの操作にはさほど問題ではありませんが、大量または数多くの文字列を扱う際にはパフォーマンス上の観点から覚えておくべきことです。

## 参照:
- [String.Replace メソッド ドキュメンテーション](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8)
- [.NET における正規表現](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [安全な文字列処理のベストプラクティス](https://www.owasp.org/index.php/Data_Validation)
