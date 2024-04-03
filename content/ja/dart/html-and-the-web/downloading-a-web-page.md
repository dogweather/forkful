---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.935340-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Dart\u306F`http`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\
  \u308C\u306F\u3001HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u884C\u3046\u305F\u3081\
  \u306E\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3067\u3059\u3002\u3053\u3053\u306B\u30A6\u30A7\u30D6\u30DA\
  \u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\u305F\u3081\u306B\
  \u305D\u308C\u3092\u4F7F\u7528\u3059\u308B\u57FA\u672C\u7684\u306A\u4F8B\u3092\u793A\
  \u3057\u307E\u3059\uFF1A \u307E\u305A\u3001`pubspec.yaml`\u306B`http`\u30D1\u30C3\
  \u30B1\u30FC\u30B8\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.699996-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F`http`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u884C\u3046\u305F\u3081\u306E\u4EBA\u6C17\u306E\u3042\u308B\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\u3059\u3002\u3053\u3053\
  \u306B\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\
  \u3059\u308B\u305F\u3081\u306B\u305D\u308C\u3092\u4F7F\u7528\u3059\u308B\u57FA\u672C\
  \u7684\u306A\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A\n\n\u307E\u305A\u3001`pubspec.yaml`\u306B\
  `http`\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## どのようにして：
Dartは`http`パッケージを提供しています。これは、HTTPリクエストを行うための人気のあるサードパーティライブラリです。ここにウェブページをダウンロードするためにそれを使用する基本的な例を示します：

まず、`pubspec.yaml`に`http`パッケージを追加します：

```yaml
dependencies:
  http: ^0.13.3
```

次に、パッケージをインポートし、それを使用してウェブページのコンテンツを取得します：

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('ページをダウンロードしました：');
    print(response.body);
  } else {
    print('リクエストが失敗しました。ステータス：${response.statusCode}。');
  }
}
```

**サンプル出力**（これはウェブページの内容に基づいて変わります）：

```
ページをダウンロードしました：
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

クッキーを扱うや、ユーザーエージェントヘッダを設定するなど、より複雑なシナリオの場合、同じ`http`パッケージを使用しますが、リクエストに追加の設定を行います：

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('カスタムヘッダーでページをダウンロードしました：');
    print(response.body);
  } else {
    print('リクエストが失敗しました。ステータス：${response.statusCode}。');
  }
}
```

このようなヘッダーを使用することで、ブラウザのリクエストをより正確に模倣でき、特定の要件やスクレイピングに対する保護があるサイトを扱う際に特に役立ちます。
