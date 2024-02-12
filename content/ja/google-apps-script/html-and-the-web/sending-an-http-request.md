---
title:                "HTTPリクエストの送信"
aliases:
- /ja/google-apps-script/sending-an-http-request.md
date:                  2024-02-01T22:01:48.746182-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTTPリクエストの送信"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Google Apps ScriptでHTTPリクエストを送信することは、外部のWebサーバーやAPIにプログラムで通信を行うことを意味します。プログラマーはこれを行うことで、データを取得したり送信したりし、広大なWebリソースや機能を直接Google Apps Scriptプロジェクトに統合します。

## どのように：

Google Apps ScriptでHTTPリクエストを送信する主な方法は、`UrlFetchApp`サービスを使用することです。このサービスは、HTTP GETおよびPOSTリクエストを行うメソッドを提供します。以下は、JSONデータを取得するためのGETリクエストを行う簡単な例です：

```javascript
function fetchJsonData() {
  var url = 'https://api.example.com/data';
  var response = UrlFetchApp.fetch(url);
  var json = response.getContentText();
  var data = JSON.parse(json);
  
  Logger.log(data);
}
```

サーバーにデータを送信するのに一般的に使用されるPOSTリクエストについては、オプションパラメーターに詳細を含める必要があります：

```javascript
function postExample() {
  var url = 'https://api.example.com/post';
  var payload = {
    key1: 'value1',
    key2: 'value2'
  };
  
  var options = {
    'method' : 'post',
    'contentType': 'application/json',
    // JavaScriptオブジェクトをJSON文字列に変換
    'payload' : JSON.stringify(payload)
  };
  
  var response = UrlFetchApp.fetch(url, options);
  Logger.log(response.getContentText());
}
```

これらのスニペットは、GETおよびPOSTリクエストの基本的な実装を示しています。出力はAPIのレスポンスによって異なり、Google Apps ScriptのLoggerで確認できます。

## より深く

Google Apps Scriptの`UrlFetchApp`サービスは、その発足以来、ヘッダーの設定、ペイロードの扱い、ファイルアップロードのためのmultipart/form-dataの処理など、HTTPリクエストをより微細に制御する機能を提供して、大幅に進化しました。外部Webサービスを統合するための直接的な手段を提供する一方で、Pythonの`requests`やNode.jsのJavaScriptの`fetch`APIのような、より堅牢なバックエンド言語から来た開発者は、その機能性がいくぶん限定的だと感じるかもしれません。

特筆すべき制限の一つは、Google Apps Scriptの実行時間制限であり、長時間実行されるリクエストに影響します。さらに、`UrlFetchApp`は幅広いユースケースをカバーしていますが、OAuth認証を含むより複雑なシナリオや、非常に大きなペイロードを扱う場合は、創造的な解決策を見つけるか、追加のGoogle Cloudリソースを活用する必要があるかもしれません。

それにもかかわらず、Google Workspace開発者が遭遇するほとんどの統合において—データの自動取得から外部サービスへの更新投稿まで—`UrlFetchApp`は強力でアクセスしやすいツールを提供します。Google Apps Scriptに統合されているため、外部ライブラリや複雑なセットアップは必要なく、Google Apps Scriptの制約の中でHTTPリクエストを比較的簡単に実行できます。Web APIの風景が拡大し続ける中で、`UrlFetchApp`はGoogle Apps ScriptプログラムがGoogleのエコシステムを超えた世界と相互作用するための重要な橋渡しとして残ります。
