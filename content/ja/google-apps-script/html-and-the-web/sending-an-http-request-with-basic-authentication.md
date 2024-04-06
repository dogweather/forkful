---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:28.356810-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u3067\u57FA\u672C\u8A8D\u8A3C\u3092\
  \u4F7F\u3063\u3066HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\
  \u306B\u306F\u3001`UrlFetchApp`\u30B5\u30FC\u30D3\u30B9\u3068base64\u30A8\u30F3\u30B3\
  \u30FC\u30C9\u3055\u308C\u305F\u8A8D\u8A3C\u30D8\u30C3\u30C0\u3092\u7D44\u307F\u5408\
  \u308F\u305B\u3066\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u30B9\
  \u30C6\u30C3\u30D7\u30D0\u30A4\u30B9\u30C6\u30C3\u30D7\u306E\u30AC\u30A4\u30C9\u3067\
  \u3059\uFF1A 1. **\u8CC7\u683C\u60C5\u5831\u306E\u30A8\u30F3\u30B3\u30FC\u30C9**:\u2026"
lastmod: '2024-04-05T21:53:42.392850-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306E\u9001\u4FE1"
weight: 45
---

## 方法：
Google Apps Scriptで基本認証を使ってHTTPリクエストを送信するには、`UrlFetchApp`サービスとbase64エンコードされた認証ヘッダを組み合わせて使用します。こちらがステップバイステップのガイドです：

1. **資格情報のエンコード**: 最初に、ユーザー名とパスワードをbase64でエンコードします。Google Apps Scriptには文字列のためのネイティブなbase64エンコーディング関数がないため、この目的には`Utilities.base64Encode`を使用します。

```javascript
var username = 'YourUsername';
var password = 'YourPassword';
var encodedCredentials = Utilities.base64Encode(username + ':' + password);
```

2. **リクエストオプションの設定**: エンコードされた資格情報が準備できたら、HTTPリクエスト用のオプションオブジェクトを準備します。これには、メソッドとヘッダが含まれます。

```javascript
var options = {
  method: 'get', // または 'post', 'put', 必要に応じて
  headers: {
    'Authorization': 'Basic ' + encodedCredentials
  }
  // エラーハンドリングのための 'muteHttpExceptions' のような追加のオプションもここに追加できます
};
```

3. **リクエストの実行**: 対象のURLとオプションオブジェクトを使って、`UrlFetchApp.fetch`メソッドを使用します。

```javascript
var url = 'https://example.com/api/resource';
var response = UrlFetchApp.fetch(url, options);
Logger.log(response.getContentText());
```

成功したリクエストのサンプル出力は、APIのレスポンスに基づいて変わります。JSONベースのAPIでは、次のようなものが表示されるかもしれません：

```
{"status":"Success","data":"Resource data here..."}
```

応答コードをチェックするか、より制御されたエラー管理のために`muteHttpExceptions`オプションを使用することで、可能なHTTPエラーを適切に処理してください。

## 深掘り
多くのプログラミング言語で、認証が必要なWebベースのリソースにアクセスするための標準的な方法として、基本認証を使ってHTTPリクエストを送信することが長年にわたって行われてきました。Google Apps Scriptの文脈では、`UrlFetchApp`はこれらのHTTPリクエスト、特に認証を必要とするものを実行するための直感的な方法を提供します。リクエストヘッダーに基本的な資格情報を含めることはシンプルかつ効果的な方法ですが、資格情報が平文で送信されるため（単にbase64エンコードされているだけで、傍受された場合簡単にデコードされます）、セキュリティ上の注意が必要です。

セキュリティを向上させるためには、特に機密データや操作を扱う場合には、OAuth 2.0のような代替手段が推奨されます。Google Apps Scriptは、このプロトコルをサポートするサービスに対して認証を行うプロセスを簡素化する`OAuth2`ライブラリを組み込んでサポートしています。

そのセキュリティの限界にもかかわらず、基本認証はインターネット全体に公開されていない単純または内部アプリケーションで広く使用されています。適切に設定されたヘッダーを持つ単一のリクエストのみが必要であり、クイックな統合やより高いセキュリティメソッドが利用不可能なAPIにとって魅力的なオプションです。しかし、プログラマーはセキュリティへの影響を考慮し、利用可能な場合はより安全な代替手段を探ることが促されます。
