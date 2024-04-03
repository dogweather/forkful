---
date: 2024-01-26 03:38:53.176588-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.107165-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u6587\u5B57\u5217\u304B\u3089\u30AF\u30A9\u30FC\u30C8\u3092\u524A\
  \u9664\u3059\u308B\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\u3093\u3067\
  \u3044\u308B\u5384\u4ECB\u306A\u4E8C\u91CD\uFF08`\"`\uFF09\u307E\u305F\u306F\u5358\
  \u4E00\uFF08`'`\uFF09\u306E\u30AF\u30A9\u30FC\u30C8\u6587\u5B57\u3092\u53D6\u308A\
  \u9664\u304F\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C7\u30FC\u30BF\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\
  \u30C3\u30D7\u3059\u308B\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u3078\u306E\u5165\
  \u529B\u306E\u6E96\u5099\u3092\u3059\u308B\u3001\u307E\u305F\u306F\u6587\u5B57\u5217\
  \u3092\u3055\u3089\u306A\u308B\u51E6\u7406\u306E\u305F\u3081\u306B\u5B89\u5168\u306B\
  \u3059\u308B\u306A\u3069\u306E\u76EE\u7684\u3067\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002\u305D\u3046\u3059\u308B\u3053\u3068\u3067\u3001\u8FF7\u3044\u30AF\u30A9\
  \u30FC\u30C8\u304C\u73FE\u308C\u305F\u3068\u304D\u306B\u7269\u4E8B\u304C\u304A\u304B\
  \u3057\u304F\u306A\u308B\u306E\u3092\u9632\u304E\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 何となぜ?
C#で文字列からクォートを削除するとは、テキストを囲んでいる厄介な二重（`"`）または単一（`'`）のクォート文字を取り除くことを意味します。プログラマーは、データをクリーンアップする、データベースへの入力の準備をする、または文字列をさらなる処理のために安全にするなどの目的でこれを行います。そうすることで、迷いクォートが現れたときに物事がおかしくなるのを防ぎます。

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
