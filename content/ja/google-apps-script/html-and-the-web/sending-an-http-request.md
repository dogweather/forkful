---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:48.746182-07:00
description: "Google Apps Script\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\
  \u4FE1\u3059\u308B\u3053\u3068\u306F\u3001\u5916\u90E8\u306EWeb\u30B5\u30FC\u30D0\
  \u30FC\u3084API\u306B\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u901A\u4FE1\u3092\u884C\
  \u3046\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30C7\u30FC\
  \u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u9001\u4FE1\u3057\u305F\u308A\u3057\u3001\
  \u5E83\u5927\u306AWeb\u30EA\u30BD\u30FC\u30B9\u3084\u6A5F\u80FD\u3092\u76F4\u63A5\
  Google Apps Script\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306B\u7D71\u5408\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-03-11T00:14:15.040987-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\
  \u3059\u308B\u3053\u3068\u306F\u3001\u5916\u90E8\u306EWeb\u30B5\u30FC\u30D0\u30FC\
  \u3084API\u306B\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u901A\u4FE1\u3092\u884C\u3046\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30C7\u30FC\u30BF\
  \u3092\u53D6\u5F97\u3057\u305F\u308A\u9001\u4FE1\u3057\u305F\u308A\u3057\u3001\u5E83\
  \u5927\u306AWeb\u30EA\u30BD\u30FC\u30B9\u3084\u6A5F\u80FD\u3092\u76F4\u63A5Google\
  \ Apps Script\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306B\u7D71\u5408\u3057\u307E\u3059\
  \u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
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
