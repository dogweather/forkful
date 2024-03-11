---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.908179-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.299140-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## 何となぜ？

DartでHTTPリクエストを送信するプロセスは、DartアプリケーションからウェブサーバーやAPIへの通信を開始することを指します。プログラマーは、ウェブからデータを取得したり、フォームを送信したり、RESTfulサービスとやり取りしたりするためにこれを行います。これは、Dartでのウェブ、サーバーサイド、モバイルアプリケーション開発において基本的な操作です。

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
