---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.908179-07:00
description: "\u65B9\u6CD5: Dart\u306B\u306F\u3001HTTP\u30EA\u30BD\u30FC\u30B9\u3092\
  \u6271\u3046\u305F\u3081\u306E\u5F37\u529B\u3067\u4FBF\u5229\u306A\u65B9\u6CD5\u3068\
  \u3057\u3066`http`\u30D1\u30C3\u30B1\u30FC\u30B8\u304C\u542B\u307E\u308C\u3066\u3044\
  \u307E\u3059\u3002\u6700\u521D\u306B\u3001pubspec.yaml\u30D5\u30A1\u30A4\u30EB\u306B\
  \u305D\u308C\u3092\u542B\u3081\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.695478-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306B\u306F\u3001HTTP\u30EA\u30BD\u30FC\u30B9\u3092\u6271\u3046\u305F\
  \u3081\u306E\u5F37\u529B\u3067\u4FBF\u5229\u306A\u65B9\u6CD5\u3068\u3057\u3066`http`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002\u6700\
  \u521D\u306B\u3001pubspec.yaml\u30D5\u30A1\u30A4\u30EB\u306B\u305D\u308C\u3092\u542B\
  \u3081\u307E\u3059\uFF1A."
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
