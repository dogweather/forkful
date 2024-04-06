---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:53.611816-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Google Apps Script\u3067\u306F\u3001\
  `UrlFetchApp`\u30B5\u30FC\u30D3\u30B9\u304CWeb\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\
  \u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u305F\u3081\u306E\u91CD\u8981\u306A\
  \u3082\u306E\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001Web\u30DA\u30FC\u30B8\u306E\
  HTML\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u53D6\u5F97\u3057\u3066\u30ED\u30B0\u306B\
  \u8A18\u9332\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3059\u3001\u30B9\u30C6\u30C3\u30D7\
  \u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u306E\u30AC\u30A4\u30C9\u3068\u30B7\u30F3\u30D7\
  \u30EB\u306A\u4F8B\u3067\u3059\uFF1A 1.\u2026"
lastmod: '2024-04-05T21:53:42.391345-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## どのように：
Google Apps Scriptでは、`UrlFetchApp`サービスがWebコンテンツをダウンロードするための重要なものです。以下は、WebページのHTMLコンテンツを取得してログに記録する方法を示す、ステップバイステップのガイドとシンプルな例です：

1. **基本的なフェッチ操作：**

```javascript
function downloadWebPage() {
  var url = "http://example.com";
  var response = UrlFetchApp.fetch(url);
  var content = response.getContentText();
  Logger.log(content);
}
```

- このコードは、example.comのHTMLコンテンツを取得してログに記録します。追加パラメーターなしでWebページのソースを取得する直接的なデモンストレーションです。

2. **リダイレクトとHTTPSの処理：**

HTTPSやリダイレクトを処理する場合、コードは大部分同じままですが、エラー処理やリダイレクトの特定のオプションを実装することを検討してください：

```javascript
function downloadSecureWebPage() {
  var options = {
    'followRedirects': true, // リダイレクトを自動的にフォロー
    'muteHttpExceptions': true // 可能性のある例外をミュートし、上品に処理
  };
  
  var url = "https://example.com";
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

3. **レート制限とクォータ：**

Google Apps Scriptのクォータに注意してください。多用する場合は、レート制限に関するエラー処理が必要になることがあります。

## 深堀り
歴史的に、Webコンテンツのダウンロードと操作は、単純なHTTPリクエストから始まり、スクリプト言語の登場で大きく進化しました。Google Apps Scriptは、G Suiteエコシステム内でこのようなタスクを直接実行することを可能にし、Googleの強力なインフラストラクチャーを活用します。`UrlFetchApp`サービスはこの機能のコア要素であり、複雑なHTTP/Sリクエストをよりシンプルなアプリケーションレベルのインターフェースにカプセル化します。

その便利さにも関わらず、Google Apps Scriptは、実行時間の制限とGoogleによって課されるクォータのため、重いWebスクレイピングを行う場合や、取得したデータの複雑な後処理が必要な場合に常に最適なツールではありません。そのような場合は、PuppeteerやCheerioのようなライブラリを使ったNode.jsなど、非同期I/O操作に設計された専用のWebスクレイピングフレームワークや言語が、より多くの柔軟性とパワーを提供するかもしれません。

さらに、Google Apps ScriptはGoogleサービス（Sheets、Docs、Driveなど）との統合や軽量なデータフェッチ操作には優れたツールですが、その実行環境の制限を念頭に置くことが重要です。集中的なタスクのためには、Google Cloud Functionsや外部のコンピュートリソースを使用したApps Scriptの高度なサービスを使用することを検討してください。
