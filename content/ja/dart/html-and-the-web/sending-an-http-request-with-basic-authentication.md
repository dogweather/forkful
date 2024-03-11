---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:31.777024-07:00
description: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046HTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u9001\u4FE1\u3059\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\
  \u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u6DFB\
  \u4ED8\u3057\u3066\u30E6\u30FC\u30B6\u30FC\u306E\u8EAB\u5143\u3092\u78BA\u8A8D\u3059\
  \u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u8A8D\u8A3C\u304C\u5FC5\u8981\u306A\u30EA\u30BD\u30FC\u30B9\u306B\
  \u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\
  \u3057\u3001\u30AF\u30E9\u30A4\u30A2\u30F3\u30C8\u3068\u30B5\u30FC\u30D0\u30FC\u9593\
  \u306E\u5B89\u5168\u306A\u901A\u4FE1\u3092\u4FDD\u8A3C\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.304124-06:00'
model: gpt-4-0125-preview
summary: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F34\u3046HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u4FE1\u3059\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u6DFB\u4ED8\
  \u3057\u3066\u30E6\u30FC\u30B6\u30FC\u306E\u8EAB\u5143\u3092\u78BA\u8A8D\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u8A8D\u8A3C\u304C\u5FC5\u8981\u306A\u30EA\u30BD\u30FC\u30B9\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\u7528\u3057\
  \u3001\u30AF\u30E9\u30A4\u30A2\u30F3\u30C8\u3068\u30B5\u30FC\u30D0\u30FC\u9593\u306E\
  \u5B89\u5168\u306A\u901A\u4FE1\u3092\u4FDD\u8A3C\u3057\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u7528\u3044\u305FHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## 何となぜ？

基本認証を伴うHTTPリクエストを送信するとは、ユーザー名とパスワードをリクエストに添付してユーザーの身元を確認することを意味します。プログラマーは認証が必要なリソースにアクセスするためにこれを使用し、クライアントとサーバー間の安全な通信を保証します。

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
