---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:03.819593-07:00
description: "\u2026"
lastmod: '2024-02-25T18:49:40.160478-07:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
---

{{< edit_this_page >}}

## 何となぜ？
C#で標準エラー（stderr）に書き込むことは、エラーメッセージや診断を通常の出力（stdout）から別にし、使い手や開発者が通常のプログラム出力とエラー通知を区別できるようにすることを意味します。プログラマーはこれを行うことで、デバッグやログ記録をより効率的にし、アプリケーションのスムーズな運用と保守を可能にします。

## 方法：
C#で標準エラーに書き込むには、`Console.Error`ストリームを使用できます。このストリームは、エラーメッセージと診断のために特別に使用されます。基本的な例を以下に示します。

```csharp
Console.Error.WriteLine("Error: Failed to process the request.");
```

標準エラーへのサンプル出力：
```
Error: Failed to process the request.
```

`Serilog` や `NLog` のように、高度なログ機能を提供するサードパーティのライブラリを使用している場面では、これらのライブラリを構成してエラーログをstderrに書き出すことができます。これらの例は単純なコンソールリダイレクションに焦点を当てていますが、本番アプリケーションでは、ログフレームワークがより堅牢なエラー処理と出力オプションを提供することを覚えておいてください。`Serilog`を使用した簡単な例を以下に示します。

まず、SerilogパッケージとそのConsoleシンクをインストールします：

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

次に、Serilogを標準エラーに書き込むように構成します：

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("This is a normal message.");
Log.Error("This is an error message.");
```

エラーメッセージの標準エラーへのサンプル出力：
```
[15:04:20 ERR] This is an error message.
```

注：SerilogのConsoleシンクでの`standardErrorFromLevel`の設定は、指定されたレベル（この場合はError）またはそれ以上のすべてのログイベントを標準エラーストリームにリダイレクトし、情報のような低レベルのメッセージは標準出力ストリームに書き込まれます。
