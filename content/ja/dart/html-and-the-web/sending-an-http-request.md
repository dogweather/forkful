---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.908179-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.695478-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\
  \u30D7\u30ED\u30BB\u30B9\u306F\u3001Dart\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u304B\u3089\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u3084API\u3078\u306E\
  \u901A\u4FE1\u3092\u958B\u59CB\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A6\u30A7\u30D6\u304B\u3089\
  \u30C7\u30FC\u30BF\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30D5\u30A9\u30FC\u30E0\
  \u3092\u9001\u4FE1\u3057\u305F\u308A\u3001RESTful\u30B5\u30FC\u30D3\u30B9\u3068\u3084\
  \u308A\u53D6\u308A\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001Dart\u3067\u306E\u30A6\u30A7\
  \u30D6\u3001\u30B5\u30FC\u30D0\u30FC\u30B5\u30A4\u30C9\u3001\u30E2\u30D0\u30A4\u30EB\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u958B\u767A\u306B\u304A\u3044\u3066\
  \u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3067\u3059\u3002."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

## 方法:
Dartには、HTTPリソースを扱うための強力で便利な方法として`http`パッケージが含まれています。最初に、pubspec.yamlファイルにそれを含めます：

```yaml
dependencies:
  http: ^0.13.3
```

次に、Dartコードでインポートして、リクエストの作成を開始します：

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  if (response.statusCode == 200) {
    print('レスポンスボディ: ${response.body}');
  } else {
    print('リクエスト失敗 ステータス: ${response.statusCode}.');
  }
}
```

成功したリクエストのサンプル出力は次のようになります：

```
レスポンスボディ: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

JSONボディを含むより複雑なリクエスト、例えばPOSTリクエストを行う場合、以下のようにします：

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  if (response.statusCode == 201) {
    print('レスポンスステータス: ${response.statusCode}');
    print('レスポンスボディ: ${response.body}');
  } else {
    print('新しい投稿の作成に失敗。ステータス: ${response.statusCode}');
  }
}
```

POSTリクエストのサンプル出力は、以下のようになる可能性があります：

```
レスポンスステータス: 201
レスポンスボディ: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

これらの例は、`http`パッケージを使ってDartで基本的なHTTP GETおよびPOSTリクエストを行う方法を紹介しています。このパッケージは、ヘッダーやボディコンテンツを含むより複雑なシナリオを含め、HTTPリクエストを送信するためのほとんどのニーズをカバーしています。
