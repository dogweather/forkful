---
title:                "基本認証を用いたHTTPリクエストの送信"
date:                  2024-03-08T21:56:31.777024-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
