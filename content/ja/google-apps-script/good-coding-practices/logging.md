---
title:                "ロギング"
aliases:
- /ja/google-apps-script/logging/
date:                  2024-02-01T21:56:17.369578-07:00
model:                 gpt-4-0125-preview
simple_title:         "ロギング"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/logging.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ?

プログラミングにおけるロギングとは、実行時のイベント、エラー、または注目すべき発生を記録することを指します。プログラマーは、問題のデバッグ、パフォーマンスの監視、および運用データの記録を行うためにこれを行います。これは、本番環境のソフトウェアの動作を維持し理解するために重要です。

## 方法:

Google Apps Scriptでは、`Logger` クラスや `console.log()` など、さまざまな方法を使用してロギングを実行できます。Logger クラスは伝統的な方法であり、シンプルなデバッグや開発目的に適しています。最近のアップデートでは、`console.log()` は Stackdriver ロギングとの統合と柔軟性を提供し、Google Cloud PlatformでのApps Scriptsの監視により堅牢なソリューションを提供します。

**Logger の使用:**

```javascript
function logSample() {
  Logger.log('これはシンプルなログメッセージです');
  
  var value = 5;
  Logger.log('値は: %s', value); // 文字列のフォーマッティング
}

// ログを表示するには:
// 1. logSample関数を実行します。
// 2. 表示 -> ログ
```

**Loggerのサンプル出力:**

```
[22-04-20 10:00:00:000 PDT] これはシンプルなログメッセージです
[22-04-20 10:00:00:001 PDT] 値は: 5
```

**console.log() の使用:**

```javascript
function consoleLogSample() {
  console.log('このメッセージはStackdriver Loggingに送られます');
  const obj = {name: 'Jane', role: 'Developer'};
  console.info('オブジェクトをログに記録:', obj);
}

// ログはGoogle Cloud Platform (GCP) のコンソールで Stackdriver Loggingの下で見ることができます。
```

**console.log()のサンプル出力:**

```
このメッセージはStackdriver Loggingに送られます
オブジェクトをログに記録: {name: "Jane", role: "Developer"}
```

`console.log()` への移行により、開発者はGCPによって提供される強力なフィルターやツールを使用して、ログを効率的に解析し分析することができ、これは伝統的なLoggerクラスでは直感的ではありません。

## 詳細な解説:

Google Apps Scriptのロギングは、大きく進化しました。初期には、`Logger` クラスが開発者がスクリプトをデバッグするための主要な方法でした。これはシンプルで、基本的なスクリプトには十分ですが、ログの検索や時間の経過によるログトレンドの分析など、現代のクラウドアプリケーションに必要な機能には欠けています。

`console.log()` の導入により、このギャップが埋められました。Google Apps ScriptのロギングをGoogle CloudのStackdriver ロギング（現在はOperations Suiteと呼ばれています）と統合し、アプリケーションのロギング、監視、およびデバッグのための中央集権的なプラットフォームを提供しました。これにより、スケールでのロギングだけでなく、ログベースのメトリクス、リアルタイムのログ解析、他のGoogle Cloudサービスとの統合など、高度なログ管理機能が利用可能になりました。

`Logger`は依然として小規模なスクリプトでの迅速なデバッグとロギングの目的には役立ちますが、`console.log()`を使用する進化は、スケーラブルでクラウドネイティブなアプリケーションを開発するための広範なシフトを反映しています。これは、今日のアプリケーションの複雑さとスケールに対応するツールを開発者に提供するGoogleのコミットメントを強調しています。しかし、新参者はGoogle Cloud Platformのコンセプトに慣れる必要があるやや急な学習曲線に注意する必要があります。それにもかかわらず、クラウド機能を十分に活用することを目指す開発者にとって有利です。クラウドサービスとのこの整合性は、クラウドコンピューティングの時代における堅牢でスケーラブルなロギングメカニズムの重要性を強調するソフトウェア開発の広範なトレンドの一部です。
