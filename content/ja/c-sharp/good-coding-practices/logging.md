---
title:                "ロギング"
aliases: - /ja/c-sharp/logging.md
date:                  2024-01-26T01:01:47.469501-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/logging.md"
---

{{< edit_this_page >}}

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
