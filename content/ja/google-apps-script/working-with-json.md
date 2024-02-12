---
title:                "JSONとの作業"
aliases:
- ja/google-apps-script/working-with-json.md
date:                  2024-02-01T22:05:51.127218-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/working-with-json.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
