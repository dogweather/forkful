---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:20.699709-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308BHTML\u306E\
  \u89E3\u6790\u306F\u3001HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u30C7\
  \u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u516C\u5F0FAPI\u304C\
  \u5229\u7528\u3067\u304D\u306A\u3044\u5834\u5408\u3067\u3082\u3001\u30A6\u30A7\u30D6\
  \u30B3\u30F3\u30C6\u30F3\u30C4\u3068\u3084\u308A\u53D6\u308A\u3092\u3057\u305F\u308A\
  \u3001\u60C5\u5831\u62BD\u51FA\u3001\u30C6\u30B9\u30C8\u3001\u3042\u308B\u3044\u306F\
  \u81EA\u52D5\u5316\u306E\u76EE\u7684\u3067\u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.698039-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308BHTML\u306E\
  \u89E3\u6790\u306F\u3001HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u30C7\
  \u30FC\u30BF\u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u516C\u5F0FAPI\u304C\
  \u5229\u7528\u3067\u304D\u306A\u3044\u5834\u5408\u3067\u3082\u3001\u30A6\u30A7\u30D6\
  \u30B3\u30F3\u30C6\u30F3\u30C4\u3068\u3084\u308A\u53D6\u308A\u3092\u3057\u305F\u308A\
  \u3001\u60C5\u5831\u62BD\u51FA\u3001\u30C6\u30B9\u30C8\u3001\u3042\u308B\u3044\u306F\
  \u81EA\u52D5\u5316\u306E\u76EE\u7684\u3067\u30A6\u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\
  \u30C4\u3092\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "HTML\u306E\u69CB\u6587\u89E3\u6790"
weight: 43
---

## 方法：
Dartでは、そのコアライブラリにHTML解析をサポートする機能を組み込んでいません。しかし、`html`のようなサードパーティパッケージを使用してHTMLドキュメントを解析し、操作することができます。

まず、`html`パッケージを`pubspec.yaml`ファイルに追加します：

```yaml
dependencies:
  html: ^0.15.0
```

次に、パッケージをDartファイルにインポートします：

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

以下は、HTMLを含む文字列を解析し、データを抽出する基本的な例です：

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hello, Dart!</h1>
      <p>これはサンプルHTMLの段落です</p>
    </body>
  </html>
  """;

  // HTML文字列を解析
  Document document = parse(htmlDocument);

  // データを抽出
  String title = document.querySelector('h1')?.text ?? "タイトルが見つかりません";
  String paragraph = document.querySelector('p')?.text ?? "段落が見つかりません";

  print('タイトル: $title');
  print('段落: $paragraph');
}
```

出力：

```
タイトル: Hello, Dart!
段落: これはサンプルHTMLの段落です
```

実際のウェブページとやり取りする場合は、HTTPリクエスト（`http`パッケージを使用してウェブコンテンツをフェッチすること）と`html`解析を組み合わせることがあります。ここに簡単な例を示します：

まず、`html`とともに`http`パッケージを追加します：

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

次に、ウェブからHTMLページをフェッチして解析します：

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // ウェブページをフェッチ
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // ページに興味のある<h1>タグがあると仮定
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('ヘッドライン: $headlines');
  } else {
    print('リクエストはステータスで失敗しました: ${response.statusCode}。');
  }
}
```

注意：上で示されたウェブスクレイピング技術は、ウェブサイトの利用規約に従って責任を持って使用すべきです。
