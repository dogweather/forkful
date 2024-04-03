---
date: 2024-01-20 17:52:33.183135-07:00
description: "How to: / \u65B9\u6CD5 ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.127285-06:00'
model: gpt-4-1106-preview
summary: .
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
