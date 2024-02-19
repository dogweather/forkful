---
aliases:
- /ja/c-sharp/printing-debug-output/
date: 2024-01-20 17:52:33.183135-07:00
description: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3068\u306F\u3001\u30B3\u30FC\u30C9\
  \u306E\u52D5\u4F5C\u3092\u76E3\u8996\u3057\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u8868\u793A\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u554F\u984C\u306E\u539F\u56E0\
  \u3092\u7279\u5B9A\u3057\u3001\u30B3\u30FC\u30C9\u304C\u610F\u56F3\u3057\u305F\u901A\
  \u308A\u306B\u52D5\u4F5C\u3057\u3066\u3044\u308B\u304B\u3092\u78BA\u304B\u3081\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.920237
model: gpt-4-1106-preview
summary: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3068\u306F\u3001\u30B3\u30FC\u30C9\
  \u306E\u52D5\u4F5C\u3092\u76E3\u8996\u3057\u7406\u89E3\u3059\u308B\u305F\u3081\u306B\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u8868\u793A\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u554F\u984C\u306E\u539F\u56E0\
  \u3092\u7279\u5B9A\u3057\u3001\u30B3\u30FC\u30C9\u304C\u610F\u56F3\u3057\u305F\u901A\
  \u308A\u306B\u52D5\u4F5C\u3057\u3066\u3044\u308B\u304B\u3092\u78BA\u304B\u3081\u308B\
  \u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

デバッグ出力とは、コードの動作を監視し理解するためにメッセージを表示することです。プログラマーは、問題の原因を特定し、コードが意図した通りに動作しているかを確かめるためにこれを行います。

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
