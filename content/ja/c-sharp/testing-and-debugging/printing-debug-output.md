---
title:                "デバッグ出力を表示する"
aliases: - /ja/c-sharp/printing-debug-output.md
date:                  2024-01-20T17:52:33.183135-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/printing-debug-output.md"
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
