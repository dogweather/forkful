---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:53.611816-07:00
description: "Google Apps\u2026"
lastmod: '2024-03-11T00:14:15.043572-06:00'
model: gpt-4-0125-preview
summary: "Google Apps\u2026"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## 何となぜ？

Google Apps ScriptにおけるWebページのダウンロードは、Webスクレイピング、データ抽出、または変更の監視などの目的で、HTMLを介してWebページのコンテンツを取得することを含みます。プログラマーは、データ収集または統合タスクを自動化し、手動の労力を最小限に抑え、リアルタイムのデータ処理を保証するために、この操作を選択します。

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
