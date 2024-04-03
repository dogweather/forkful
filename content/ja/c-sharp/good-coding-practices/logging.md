---
date: 2024-01-26 01:01:47.469501-07:00
description: "\u30ED\u30B0\u3068\u306F\u3001\u30E9\u30F3\u30BF\u30A4\u30E0\u4E2D\u306E\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u30A4\u30D9\u30F3\u30C8\u3084\
  \u30C7\u30FC\u30BF\u51FA\u529B\u3092\u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D0\u30B0\u306E\
  \u8A3A\u65AD\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u6027\u80FD\u306E\u76E3\u8996\
  \u3001\u30E6\u30FC\u30B6\u30FC\u30A2\u30AF\u30B7\u30E7\u30F3\u306E\u8FFD\u8DE1\u3001\
  \u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u3084\u30D3\u30B8\u30CD\u30B9\u57FA\u6E96\u306E\
  \u30B3\u30F3\u30D7\u30E9\u30A4\u30A2\u30F3\u30B9\u3092\u7DAD\u6301\u3059\u308B\u305F\
  \u3081\u306B\u30ED\u30B0\u3092\u53D6\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.132849-06:00'
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u3068\u306F\u3001\u30E9\u30F3\u30BF\u30A4\u30E0\u4E2D\u306E\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u30A4\u30D9\u30F3\u30C8\u3084\
  \u30C7\u30FC\u30BF\u51FA\u529B\u3092\u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30D0\u30B0\u306E\
  \u8A3A\u65AD\u3001\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u6027\u80FD\u306E\u76E3\u8996\
  \u3001\u30E6\u30FC\u30B6\u30FC\u30A2\u30AF\u30B7\u30E7\u30F3\u306E\u8FFD\u8DE1\u3001\
  \u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u3084\u30D3\u30B8\u30CD\u30B9\u57FA\u6E96\u306E\
  \u30B3\u30F3\u30D7\u30E9\u30A4\u30A2\u30F3\u30B9\u3092\u7DAD\u6301\u3059\u308B\u305F\
  \u3081\u306B\u30ED\u30B0\u3092\u53D6\u308A\u307E\u3059\u3002."
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

## 何となぜ？
ログとは、ランタイム中のアプリケーションのイベントやデータ出力を記録するプロセスです。プログラマーは、バグの診断、ソフトウェア性能の監視、ユーザーアクションの追跡、セキュリティやビジネス基準のコンプライアンスを維持するためにログを取ります。

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
