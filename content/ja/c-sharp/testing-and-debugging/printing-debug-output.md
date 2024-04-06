---
date: 2024-01-20 17:52:33.183135-07:00
description: "How to: / \u65B9\u6CD5 \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u6614\
  \u304B\u3089\u3042\u308B\u6A5F\u80FD\u3067\u3059\u3002\u8A00\u8A9E\u306B\u3088\u3063\
  \u3066\u306F `print` \u3084 `println` \u306A\u3069\u3067\u51FA\u529B\u3057\u307E\
  \u3059\u304C\u3001C#\u3067\u306F `Console.WriteLine` \u3092\u4F7F\u3063\u3066\u30B3\
  \u30F3\u30BD\u30FC\u30EB\u306B\u66F8\u304D\u51FA\u3057\u307E\u3059\u3002\u4EE3\u308F\
  \u308A\u306B\u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\u3092\u4F7F\u7528\u3059\u308B\u3053\
  \u3068\u3082\u3042\u308A\u307E\u3059\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.057376-06:00'
model: gpt-4-1106-preview
summary: "/ \u65B9\u6CD5 \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u6614\u304B\u3089\
  \u3042\u308B\u6A5F\u80FD\u3067\u3059\u3002\u8A00\u8A9E\u306B\u3088\u3063\u3066\u306F\
  \ `print` \u3084 `println` \u306A\u3069\u3067\u51FA\u529B\u3057\u307E\u3059\u304C\
  \u3001C#\u3067\u306F `Console.WriteLine` \u3092\u4F7F\u3063\u3066\u30B3\u30F3\u30BD\
  \u30FC\u30EB\u306B\u66F8\u304D\u51FA\u3057\u307E\u3059\u3002\u4EE3\u308F\u308A\u306B\
  \u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3082\
  \u3042\u308A\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to: / 方法
```C#
using System;

class DebugExample
{
    static void Main()
    {
        // 標準的なデバッグ出力
        Console.WriteLine("Debug: Start of Main");
        
        // エラーを模擬するための変数
        int zero = 0;
        
        try
        {
            int result = 10 / zero;
        }
        catch (DivideByZeroException ex)
        {
            // エラーメッセージの出力
            Console.WriteLine($"Error: {ex.Message}");
        }
        
        // プログラムの終わりを知らせる
        Console.WriteLine("Debug: End of Main");
    }
}
```

**Sample Output:**
```
Debug: Start of Main
Error: Attempted to divide by zero.
Debug: End of Main
```

## Deep Dive / より深く
デバッグ出力は昔からある機能です。言語によっては `print` や `println` などで出力しますが、C#では `Console.WriteLine` を使ってコンソールに書き出します。代わりにログファイルを使用することもあります。

C#では、`System.Diagnostics.Debug` クラスを使うことでデバッグ中にのみ出力されるメッセージを管理できます。例えば `Debug.WriteLine()` はリリースビルドで削除されます。

また、`Trace` クラスは `Debug` クラスと同様ですが、リリースビルドでも出力され続けるため、本番環境でもトレース情報を残すことができます。

実際の開発では、多くのアプリケーションでより進んだログライブラリ（例: `log4net` や `NLog`）も使われます。これらは出力をカスタマイズしたり、異なる出力先（ファイル、データベース等）を選択できます。

## See Also / 関連情報
- [Microsoft Documentation: Debug Class](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.debug)
- [Microsoft Documentation: Trace Class](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.trace)
