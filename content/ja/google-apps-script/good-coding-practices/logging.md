---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:17.369578-07:00
description: "\u65B9\u6CD5: Google Apps Script\u3067\u306F\u3001`Logger` \u30AF\u30E9\
  \u30B9\u3084 `console.log()` \u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u65B9\
  \u6CD5\u3092\u4F7F\u7528\u3057\u3066\u30ED\u30AE\u30F3\u30B0\u3092\u5B9F\u884C\u3067\
  \u304D\u307E\u3059\u3002Logger \u30AF\u30E9\u30B9\u306F\u4F1D\u7D71\u7684\u306A\u65B9\
  \u6CD5\u3067\u3042\u308A\u3001\u30B7\u30F3\u30D7\u30EB\u306A\u30C7\u30D0\u30C3\u30B0\
  \u3084\u958B\u767A\u76EE\u7684\u306B\u9069\u3057\u3066\u3044\u307E\u3059\u3002\u6700\
  \u8FD1\u306E\u30A2\u30C3\u30D7\u30C7\u30FC\u30C8\u3067\u306F\u3001`console.log()`\
  \ \u306F\u2026"
lastmod: '2024-04-05T22:37:49.787339-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u306F\u3001`Logger` \u30AF\u30E9\u30B9\u3084 `console.log()`\
  \ \u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u65B9\u6CD5\u3092\u4F7F\u7528\
  \u3057\u3066\u30ED\u30AE\u30F3\u30B0\u3092\u5B9F\u884C\u3067\u304D\u307E\u3059\u3002\
  Logger \u30AF\u30E9\u30B9\u306F\u4F1D\u7D71\u7684\u306A\u65B9\u6CD5\u3067\u3042\u308A\
  \u3001\u30B7\u30F3\u30D7\u30EB\u306A\u30C7\u30D0\u30C3\u30B0\u3084\u958B\u767A\u76EE\
  \u7684\u306B\u9069\u3057\u3066\u3044\u307E\u3059\u3002\u6700\u8FD1\u306E\u30A2\u30C3\
  \u30D7\u30C7\u30FC\u30C8\u3067\u306F\u3001`console.log()` \u306F Stackdriver \u30ED\
  \u30AE\u30F3\u30B0\u3068\u306E\u7D71\u5408\u3068\u67D4\u8EDF\u6027\u3092\u63D0\u4F9B\
  \u3057\u3001Google Cloud Platform\u3067\u306EApps Scripts\u306E\u76E3\u8996\u306B\
  \u3088\u308A\u5805\u7262\u306A\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\u30F3\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002"
title: "\u30ED\u30AE\u30F3\u30B0"
weight: 17
---

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
