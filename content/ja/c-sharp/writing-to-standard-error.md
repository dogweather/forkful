---
title:                "標準エラーへの書き込み"
date:                  2024-01-19
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"

category:             "C#"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？
標準エラーへの書き込みは、エラーメッセージや警告を出力する手段だ。これを使う理由は、標準出力（プログラムのメインの出力）と分けることで、ログの整理やデバッグをしやすくするためだ。

## How to: / 方法
```csharp
using System;

class Program
{
    static void Main()
    {
        Console.Error.WriteLine("エラー: 何か問題が発生しました。");
        Console.WriteLine("通常の出力");
    }
}
```
出力例:
```
> dotnet run
エラー: 何か問題が発生しました。
通常の出力
```
標準エラー出力が先に表示されることもあるが、実際は出力の順番は実行環境によって変わる点に注意。

## Deep Dive / 詳細な情報
コマンドラインツールが始まった頃、標準出力と標準エラーを別々にし、エラーメッセージをフィルタリングする利点が見つかった。C#では、`System.Console.Error`は`TextWriter`オブジェクトであり、`Console.WriteLine`と同じ使い方ができる。ただ、`Error`を使うと、エラーメッセージは標準エラーストリームに流れる。これにより、ログファイルやコンソールでエラーを分けて扱える。別の方法として、`System.Diagnostics.Trace`やロギングフレームワークの使用も考えられるが、シンプルなスクリプトや小規模アプリでは`Console.Error`で十分だ。

## See Also / 参照
- .NET API Documentation: [Console.Error Property](https://docs.microsoft.com/dotnet/api/system.console.error)
- Microsoft C# Guide: [Console Class](https://docs.microsoft.com/dotnet/csharp/programming-guide/main-and-command-args/)
- Stack Overflow: [Differences between Console.WriteLine and Console.Error.WriteLine](https://stackoverflow.com/questions/138052/difference-between-console-write-and-console-error-write)
