---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:31.777024-07:00
description: "\u65B9\u6CD5: Dart\u3067\u306F\u3001`http`\u30D1\u30C3\u30B1\u30FC\u30B8\
  \u3092\u4F7F\u7528\u3057\u3066\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046HTTP\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3067\u304D\u307E\u3059\u3002\u307E\u305A\
  \u3001`pubspec.yaml`\u30D5\u30A1\u30A4\u30EB\u306B`http`\u30D1\u30C3\u30B1\u30FC\
  \u30B8\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.701664-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306F\u3001`http`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\
  \u3057\u3066\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046HTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u9001\u4FE1\u3067\u304D\u307E\u3059\u3002\u307E\u305A\u3001`pubspec.yaml`\u30D5\
  \u30A1\u30A4\u30EB\u306B`http`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u8FFD\u52A0\u3057\
  \u307E\u3059\uFF1A."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306E\u9001\u4FE1"
weight: 45
---

## 方法:
Dartでは、`http`パッケージを使用して基本認証を伴うHTTPリクエストを送信できます。まず、`pubspec.yaml`ファイルに`http`パッケージを追加します：

```yaml
dependencies:
  http: ^0.13.4
```

次に、Dartファイルでパッケージをインポートします：

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

基本認証を伴うGETリクエストを送信するには、次のコードを使用できます：

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('ユーザーデータが正常に取得されました！');
    print('レスポンスボディ：${response.body}');
  } else {
    print('ステータスコード：${response.statusCode}でユーザーデータの取得に失敗しました');
  }
}
```

このコードは、基本認証ヘッダーを使用して'https://yourapi.com/userdata' にGETリクエストを送信します。ユーザー名とパスワードはbase64でエンコードされ、基本アクセス認証基準に従って'Authorization'ヘッダーに渡されます。

**サンプル出力：**

リクエストが成功し、サーバーがステータスコード200を返した場合、次のように表示されるかもしれません：

```plaintext
ユーザーデータが正常に取得されました！
レスポンスボディ：{"id":1, "name":"John Doe", "email":"john@example.com"}
```

認証に失敗した場合や他にエラーがある場合、レスポンスのステータスコードが問題を特定するのに役立ちます。
