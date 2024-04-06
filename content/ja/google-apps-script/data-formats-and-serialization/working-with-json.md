---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:51.127218-07:00
description: "\u4F7F\u3044\u65B9: Google Apps\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\u306E\
  JSON\u306E\u64CD\u4F5C\u306F\u3001JSON\u306E\u89E3\u6790\u3068\u6587\u5B57\u5217\
  \u5316\u306B\u5BFE\u3059\u308BJavaScript\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u30B5\
  \u30DD\u30FC\u30C8\u306B\u5927\u304D\u304F\u4F9D\u5B58\u3059\u308B\u305F\u3081\u3001\
  \u7C21\u5358\u306A\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\u3053\u3067\u306F\
  \u3044\u304F\u3064\u304B\u306E\u4E00\u822C\u7684\u306A\u64CD\u4F5C\u3092\u7D39\u4ECB\
  \u3057\u307E\u3059\uFF1A **1. JSON\u306E\u89E3\u6790**:\u2026"
lastmod: '2024-04-05T21:53:42.427818-06:00'
model: gpt-4-0125-preview
summary: "Google Apps\u30B9\u30AF\u30EA\u30D7\u30C8\u3067\u306EJSON\u306E\u64CD\u4F5C\
  \u306F\u3001JSON\u306E\u89E3\u6790\u3068\u6587\u5B57\u5217\u5316\u306B\u5BFE\u3059\
  \u308BJavaScript\u306E\u30CD\u30A4\u30C6\u30A3\u30D6\u30B5\u30DD\u30FC\u30C8\u306B\
  \u5927\u304D\u304F\u4F9D\u5B58\u3059\u308B\u305F\u3081\u3001\u7C21\u5358\u306A\u30D7\
  \u30ED\u30BB\u30B9\u3067\u3059\u3002\u3053\u3053\u3067\u306F\u3044\u304F\u3064\u304B\
  \u306E\u4E00\u822C\u7684\u306A\u64CD\u4F5C\u3092\u7D39\u4ECB\u3057\u307E\u3059\uFF1A\
  \ **1."
title: "JSON\u3068\u306E\u4F5C\u696D"
weight: 38
---

## 使い方:
Google AppsスクリプトでのJSONの操作は、JSONの解析と文字列化に対するJavaScriptのネイティブサポートに大きく依存するため、簡単なプロセスです。ここではいくつかの一般的な操作を紹介します：

**1. JSONの解析**: WebサービスからJSON文字列を取得したと仮定します。それをJavaScriptオブジェクトに解析することは、データ操作に不可欠です。

```javascript
var jsonString = '{"name": "Sample Project", "version": "1.0.0"}';
var obj = JSON.parse(jsonString);
Logger.log(obj.name); // 出力: Sample Project
```

**2. JavaScriptオブジェクトの文字列化**: 逆に、JavaScriptオブジェクトをJSON文字列に変換することは、Appsスクリプトから外部サービスへデータを送信する必要がある場合に有用です。

```javascript
var projectData = {
  name: "Sample Project",
  version: "1.0.0"
};
var jsonString = JSON.stringify(projectData);
Logger.log(jsonString); // 出力: '{"name":"Sample Project","version":"1.0.0"}'
```

**3. 複雑なデータの操作**:
オブジェクトの配列のようなより複雑なデータ構造の場合でも、プロセスは同じままで、データ表現のためのJSONの柔軟性を示しています。

```javascript
var projects = [
  {name: "Project 1", version: "1.0"},
  {name: "Project 2", version: "2.0"}
];
var jsonString = JSON.stringify(projects);
Logger.log(jsonString); // 出力: '[{"name":"Project 1","version":"1.0"},{"name":"Project 2","version":"2.0"}]'
```

## 深堀り
JSONの現代のWebアプリケーションにおける普及度は、そのシンプルさとJavaScript、Webの言語とのシームレスな統合方法が原因で強調できません。その設計はJavaScriptオブジェクトリテラルに触発されていますが、より厳格であり、その迅速な採用を容易にします。2000年代初頭、JSONはXMLに代わるものとしてAJAX駆動のWebアプリケーション向けのより軽量で冗長性の少ないデータ交換フォーマットとして人気を博しました。さまざまなGoogle APIや外部サービスとのGoogle Appsスクリプトの深い統合を考えると、JSONはこれらのプラットフォーム間でデータを構造化し、輸送し、操作するための重要な形式として機能します。

JSONはWebアプリケーションに最適である一方で、設定ファイル用のYAMLや、高性能環境でのより効率的なバイナリシリアライゼーション用のProtobufなど、代替データフォーマットも存在します。しかし、JSONの読みやすさ、使いやすさ、およびプログラミング言語やツール間の広範囲にわたるサポートのバランスが、Google Appsスクリプトやその先に進む多くの開発者にとってのデフォルトの選択肢としての地位を固めています。
