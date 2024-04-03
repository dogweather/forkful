---
date: 2024-01-26 01:01:47.469501-07:00
description: "\u65B9\u6CD5: C#\u3067\u306F\u3001\u7D44\u307F\u8FBC\u307F\u306E`System.Diagnostics`\u540D\
  \u524D\u7A7A\u9593\u3084NLog\u3084log4net\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3067\u304D\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001.NET Core\u3067\u5229\u7528\u53EF\u80FD\
  \u306A`ILogger`\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3092\u4F7F\u7528\
  \u3057\u305F\u7C21\u5358\u306A\u4F8B\u3067\u3059."
lastmod: '2024-03-13T22:44:42.132849-06:00'
model: gpt-4-1106-preview
summary: "C#\u3067\u306F\u3001\u7D44\u307F\u8FBC\u307F\u306E`System.Diagnostics`\u540D\
  \u524D\u7A7A\u9593\u3084NLog\u3084log4net\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3067\u304D\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001.NET Core\u3067\u5229\u7528\u53EF\u80FD\
  \u306A`ILogger`\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u3092\u4F7F\u7528\
  \u3057\u305F\u7C21\u5358\u306A\u4F8B\u3067\u3059."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 方法:
C#では、組み込みの`System.Diagnostics`名前空間やNLogやlog4netのようなサードパーティライブラリを使用できます。以下は、.NET Coreで利用可能な`ILogger`インターフェイスを使用した簡単な例です:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("これは情報メッセージです。");
        logger.LogWarning("これは警告メッセージです。");
        logger.LogError("これはエラーメッセージです。");
    }
}
```

サンプル出力:
```
info: Program[0]
      これは情報メッセージです。
warn: Program[0]
      これは警告メッセージです。
fail: Program[0]
      これはエラーメッセージです。
```

## ディープダイブ
ソフトウェア開発におけるログの歴史は、ほぼプログラミング自体と同じく古く、単純なprint文から洗練された設定可能なシステムへと進化しました。元々ログは、ファイルやコンソールに書き込むことで行われていましたが、これはログ集約システムや分散トレーシングプラットフォーム（ELKスタックやJaegerなど）のようなより複雑な構造を含むように拡大しています。

.NET内蔵のロギングに代わるものには、第三者のライブラリがあります:
- **NLog**: 汎用性が高く設定も簡単で、ログのルーティング、フォーマット、フィルタリングに関する機能が豊富です。
- **log4net**: Javaのlog4jライブラリに触発されたもので、XMLからの高度な設定が可能で、様々なログリポジトリをサポートしています。

実装の詳細に関しては、ログ抽象化の選択（Microsoft.Extensions.Loggingのような）や背後にあるロギングプロバイダは、アプリケーションのパフォーマンスと信頼性に大きく影響する可能性があります。ロギングレベルを適切に設定し、ログの書き込みがボトルネックにならないようにすることが重要です。

また、文字列だけではなくキーと値のペアやオブジェクトとしてログを取る構造化ログは、より正確で実用的なログを可能にし、クエリや分析が容易になります。

## 関連情報
- [Microsoft.Extensions.Logging のドキュメント](https://docs.microsoft.com/ja-jp/aspnet/core/fundamentals/logging/)
- [NLog のドキュメント](https://nlog-project.org/documentation/)
- [log4net のドキュメント](https://logging.apache.org/log4net/)
- [Serilog のドキュメント](https://serilog.net/) （構造化ログの例として）
