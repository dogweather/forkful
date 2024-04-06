---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:03.819593-07:00
description: "\u65B9\u6CD5\uFF1A C#\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\u306B\u66F8\
  \u304D\u8FBC\u3080\u306B\u306F\u3001`Console.Error`\u30B9\u30C8\u30EA\u30FC\u30E0\
  \u3092\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u3053\u306E\u30B9\u30C8\u30EA\u30FC\
  \u30E0\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\u3068\u8A3A\u65AD\
  \u306E\u305F\u3081\u306B\u7279\u5225\u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\
  \u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:43.018790-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
