---
aliases:
- /ja/swift/logging/
date: 2024-01-26 01:09:07.177387-07:00
description: "\u30ED\u30B0\u3068\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u306E\u632F\u308B\u821E\u3044\u3001\u30A8\u30E9\u30FC\u3001\u305D\u306E\u4ED6\
  \u306E\u91CD\u8981\u306A\u60C5\u5831\u3092\u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30FC\
  \u30BF\u30D9\u30FC\u30B9\u306A\u3069\u306E\u6301\u7D9A\u7684\u306A\u5A92\u4F53\u306B\
  \u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30D7\u30EA\u306E\u5065\u5168\
  \u6027\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u8FFD\u8DE1\u3057\u3001\
  \u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u3001\u672C\u756A\u74B0\u5883\u3067\
  \u30A8\u30F3\u30B8\u30F3\u30EB\u30FC\u30E0\u306E\u4E0B\u3067\u4F55\u304C\u8D77\u3053\
  \u3063\u3066\u3044\u308B\u304B\u3092\u76E3\u8996\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.235383
model: gpt-4-1106-preview
summary: "\u30ED\u30B0\u3068\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u306E\u632F\u308B\u821E\u3044\u3001\u30A8\u30E9\u30FC\u3001\u305D\u306E\u4ED6\
  \u306E\u91CD\u8981\u306A\u60C5\u5831\u3092\u30D5\u30A1\u30A4\u30EB\u3084\u30C7\u30FC\
  \u30BF\u30D9\u30FC\u30B9\u306A\u3069\u306E\u6301\u7D9A\u7684\u306A\u5A92\u4F53\u306B\
  \u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30D7\u30EA\u306E\u5065\u5168\
  \u6027\u3084\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u8FFD\u8DE1\u3057\u3001\
  \u554F\u984C\u3092\u30C7\u30D0\u30C3\u30B0\u3057\u3001\u672C\u756A\u74B0\u5883\u3067\
  \u30A8\u30F3\u30B8\u30F3\u30EB\u30FC\u30E0\u306E\u4E0B\u3067\u4F55\u304C\u8D77\u3053\
  \u3063\u3066\u3044\u308B\u304B\u3092\u76E3\u8996\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
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
