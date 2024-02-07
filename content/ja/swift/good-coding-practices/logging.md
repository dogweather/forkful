---
title:                "ロギング"
date:                  2024-01-26T01:09:07.177387-07:00
model:                 gpt-4-1106-preview
simple_title:         "ロギング"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/logging.md"
---

{{< edit_this_page >}}

## 何となぜ？
ログとは、アプリケーションの振る舞い、エラー、その他の重要な情報をファイルやデータベースなどの持続的な媒体に記録するプロセスのことです。プログラマーは、アプリの健全性やパフォーマンスを追跡し、問題をデバッグし、本番環境でエンジンルームの下で何が起こっているかを監視するためにこれを行います。

## 方法：
Swiftでは、`print`文を使用してコンソールにログを書き出すか、AppleプラットフォームのUnified Logging Systemに接続するより柔軟な`os.log` APIを使用することができます。

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // 単純なprint文
    print("Fetch started")
    
    // os.logを使用して情報レベルのイベントをログ
    os_log(.info, log: logger, "APIからデータをフェッチしています。")
    
    do {
        let data = try performNetworkRequest()
        // デバッグレベルのイベントをログ
        os_log(.debug, log: logger, "受信したデータ：%@", data.description)
    } catch {
        // エラーレベルのイベントをログ
        os_log(.error, log: logger, "データのフェッチに失敗しました：%@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // ネットワークリクエストをシミュレート
    return Data()
}
```

コンソール上のサンプル出力は、このようになるかもしれません：

```
Fetch started
APIからデータをフェッチしています。
受信したデータ：いくつかのデータバイト...
```

エラーの場合は、以下のようになる可能性があります：

```
データのフェッチに失敗しました：インターネット接続がオフラインのようです。
```

## 深堀り
iOS 10とmacOS Sierraで導入されたUnified Logging Systemを使うと、Swiftでのログ記録は新たなパワーと効率を手に入れます。コンソールに直接出力される`print`文とは異なり、このシステムはアクティビティベースであり、重要性やビルドがデバッグ用かリリース用かに基づいてログメッセージをフィルタリングすることが可能です。

歴史的な文脈は、iOSおよびmacOSにおけるログの進化を基本的なprint文から、インストゥルメントアプリやコンソールと統合して複雑なログ分析を可能にする包括的なツールへと進展させています。

Swift内でのログ記録の代替手段には、Unified Logging System上のマクロレイヤを提供し、ログのフォーマット、ファイル管理、およびパフォーマンスオプションに関して強化された制御を提供するCocoaLumberjackのようなサードパーティライブラリがあります。

最後に、実装の詳細です。OSLogは効率的であるだけでなく、プライベートデータのロギング時の難読化が可能なプライバシー意識の高い設計となっています。フォルト、エラー、情報、デバッグの各レベルのログをカテゴライズし、トラブルシューティングのための異なる粒度を提供します。

## 参照
- [AppleのUnified Loggingのドキュメント](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlichの로그記録チュートリアル](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjackのGitHubリポジトリ](https://github.com/CocoaLumberjack/CocoaLumberjack)
