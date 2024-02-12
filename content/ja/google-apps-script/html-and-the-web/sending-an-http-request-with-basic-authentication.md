---
title:                "基本認証を用いたHTTPリクエストの送信"
aliases: - /ja/google-apps-script/sending-an-http-request-with-basic-authentication.md
date:                  2024-02-01T22:02:28.356810-07:00
model:                 gpt-4-0125-preview
simple_title:         "基本認証を用いたHTTPリクエストの送信"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
