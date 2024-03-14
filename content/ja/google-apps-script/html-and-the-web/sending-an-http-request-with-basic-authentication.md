---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:28.356810-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\u672C\u8A8D\u8A3C\u3067\
  \u9001\u4FE1\u3059\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30EA\u30AF\u30A8\u30B9\u30C8\u30D8\u30C3\u30C0\
  \u30FC\u306B\u30A8\u30F3\u30B3\u30FC\u30C9\u3057\u3001\u4FDD\u8B77\u3055\u308C\u305F\
  \u30EA\u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306E\
  \u65B9\u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B5\
  \u30FC\u30D0\u30FC\u5074\u306E\u8A8D\u8A3C\u306E\u305F\u3081\u3001\u307E\u305F\u306F\
  \u30C7\u30FC\u30BF\u53D6\u5F97\u3084\u30B3\u30F3\u30C6\u30F3\u30C4\u6295\u7A3F\u306A\
  \u3069\u306E\u64CD\u4F5C\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u8981\u6C42\u3059\u308B\
  API\u3068\u7D71\u5408\u3059\u308B\u305F\u3081\u306B\u3053\u306E\u65B9\u6CD5\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.444377-06:00'
model: gpt-4-0125-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u57FA\u672C\u8A8D\u8A3C\u3067\u9001\
  \u4FE1\u3059\u308B\u3053\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\
  \u30B9\u30EF\u30FC\u30C9\u3092\u30EA\u30AF\u30A8\u30B9\u30C8\u30D8\u30C3\u30C0\u30FC\
  \u306B\u30A8\u30F3\u30B3\u30FC\u30C9\u3057\u3001\u4FDD\u8B77\u3055\u308C\u305F\u30EA\
  \u30BD\u30FC\u30B9\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306E\u65B9\
  \u6CD5\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B5\u30FC\
  \u30D0\u30FC\u5074\u306E\u8A8D\u8A3C\u306E\u305F\u3081\u3001\u307E\u305F\u306F\u30C7\
  \u30FC\u30BF\u53D6\u5F97\u3084\u30B3\u30F3\u30C6\u30F3\u30C4\u6295\u7A3F\u306A\u3069\
  \u306E\u64CD\u4F5C\u306B\u57FA\u672C\u8A8D\u8A3C\u3092\u8981\u6C42\u3059\u308BAPI\u3068\
  \u7D71\u5408\u3059\u308B\u305F\u3081\u306B\u3053\u306E\u65B9\u6CD5\u3092\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストを基本認証で送信することは、ユーザー名とパスワードをリクエストヘッダーにエンコードし、保護されたリソースにアクセスするための方法です。プログラマーは、サーバー側の認証のため、またはデータ取得やコンテンツ投稿などの操作に基本認証を要求するAPIと統合するためにこの方法を使用します。

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
