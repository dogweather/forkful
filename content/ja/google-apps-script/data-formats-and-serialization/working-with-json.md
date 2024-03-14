---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:51.127218-07:00
description: "JSON\u3001\u307E\u305F\u306FJavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u8868\u8A18\u3001\u306F\u8EFD\u91CF\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u4FDD\
  \u5B58\u304A\u3088\u3073\u8F38\u9001\u7528\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\
  \u3042\u308A\u3001\u30B5\u30FC\u30D0\u30FC\u304B\u3089\u30AF\u30E9\u30A4\u30A2\u30F3\
  \u30C8\u3078\u306E\u901A\u4FE1\u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u7406\
  \u60F3\u7684\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\
  \u306E\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u69CB\u9020\u3068JavaScript\u30D9\
  \u30FC\u30B9\u306E\u74B0\u5883\u5185\u3067\u306E\u7C21\u5358\u306A\u7D71\u5408\u306B\
  \u3088\u308A\u3001Google\u2026"
lastmod: '2024-03-13T22:44:41.474253-06:00'
model: gpt-4-0125-preview
summary: "JSON\u3001\u307E\u305F\u306FJavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u8868\u8A18\u3001\u306F\u8EFD\u91CF\u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u4FDD\u5B58\
  \u304A\u3088\u3073\u8F38\u9001\u7528\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3042\
  \u308A\u3001\u30B5\u30FC\u30D0\u30FC\u304B\u3089\u30AF\u30E9\u30A4\u30A2\u30F3\u30C8\
  \u3078\u306E\u901A\u4FE1\u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\u7406\u60F3\
  \u7684\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u306E\
  \u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u69CB\u9020\u3068JavaScript\u30D9\
  \u30FC\u30B9\u306E\u74B0\u5883\u5185\u3067\u306E\u7C21\u5358\u306A\u7D71\u5408\u306B\
  \u3088\u308A\u3001Google\u2026"
title: "JSON\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## 何とその理由？

JSON、またはJavaScriptオブジェクト表記、は軽量形式のデータ保存および輸送用フォーマットであり、サーバーからクライアントへの通信や設定ファイルに理想的です。プログラマーは、その人間が読みやすい構造とJavaScriptベースの環境内での簡単な統合により、Google AppsスクリプトでGoogleサービス（Sheets、Docs、Driveなど）と外部ソース間のデータ交換にJSONを活用します。

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
