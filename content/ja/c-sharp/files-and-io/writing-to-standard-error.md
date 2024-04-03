---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:03.819593-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.146771-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u306B\u66F8\u304D\
  \u8FBC\u3080\u3053\u3068\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u901A\u5E38\u306E\u51FA\u529B\uFF08stdout\uFF09\u304B\u3089\
  \u5225\u306B\u3057\u3001\u4F7F\u3044\u624B\u3084\u958B\u767A\u8005\u304C\u901A\u5E38\
  \u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\u30E9\u30FC\u901A\u77E5\
  \u3092\u533A\u5225\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30ED\
  \u30B0\u8A18\u9332\u3092\u3088\u308A\u52B9\u7387\u7684\u306B\u3057\u3001\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u30B9\u30E0\u30FC\u30BA\u306A\u904B\u7528\
  \u3068\u4FDD\u5B88\u3092\u53EF\u80FD\u306B\u3057\u307E\u3059\u3002."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
