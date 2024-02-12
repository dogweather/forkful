---
title:                "文字列から引用符を削除する"
aliases: - /ja/c-sharp/removing-quotes-from-a-string.md
date:                  2024-01-26T03:38:53.176588-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

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
