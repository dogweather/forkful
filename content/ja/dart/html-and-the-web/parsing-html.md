---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:20.699709-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u3067\u306F\u3001\u305D\u306E\u30B3\u30A2\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u306BHTML\u89E3\u6790\u3092\u30B5\u30DD\u30FC\u30C8\u3059\
  \u308B\u6A5F\u80FD\u3092\u7D44\u307F\u8FBC\u3093\u3067\u3044\u307E\u305B\u3093\u3002\
  \u3057\u304B\u3057\u3001`html`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\u3057\u3066HTML\u30C9\
  \u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u89E3\u6790\u3057\u3001\u64CD\u4F5C\u3059\u308B\
  \u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002 \u307E\u305A\u3001`html`\u30D1\u30C3\
  \u30B1\u30FC\u30B8\u3092`pubspec.yaml`\u30D5\u30A1\u30A4\u30EB\u306B\u8FFD\u52A0\
  \u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.611374-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001`html`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092`pubspec.yaml`\u30D5\
  \u30A1\u30A4\u30EB\u306B\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
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
