---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:39.935340-07:00
description: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3068\u306F\u3001\u305D\u306EURL\u3092\u4ECB\u3057\u3066\u30A6\u30A7\u30D6\
  \u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3057\u3066\u51E6\u7406\u307E\
  \u305F\u306F\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u3001\u60C5\u5831\u3092\u62BD\u51FA\u3057\u305F\u308A\u3001\u5909\
  \u66F4\u3092\u76E3\u8996\u3057\u305F\u308A\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\
  \u30A2\u30FC\u30AB\u30A4\u30D6\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30A6\u30A7\u30D6\
  \u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u30DE\u30A4\u30CB\
  \u30F3\u30B0\u3001\u81EA\u52D5\u5316\u30C6\u30B9\u30C8\u306E\u30BF\u30B9\u30AF\u306B\
  \u304A\u3044\u3066\u57FA\u672C\u7684\u306A\u4F5C\u696D\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.699996-06:00'
model: gpt-4-0125-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3068\u306F\u3001\u305D\u306EURL\u3092\u4ECB\u3057\u3066\u30A6\u30A7\u30D6\
  \u30DA\u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3057\u3066\u51E6\u7406\u307E\
  \u305F\u306F\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u306F\u3001\u60C5\u5831\u3092\u62BD\u51FA\u3057\u305F\u308A\u3001\u5909\
  \u66F4\u3092\u76E3\u8996\u3057\u305F\u308A\u3001\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\
  \u30A2\u30FC\u30AB\u30A4\u30D6\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30A6\u30A7\u30D6\
  \u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u30DE\u30A4\u30CB\
  \u30F3\u30B0\u3001\u81EA\u52D5\u5316\u30C6\u30B9\u30C8\u306E\u30BF\u30B9\u30AF\u306B\
  \u304A\u3044\u3066\u57FA\u672C\u7684\u306A\u4F5C\u696D\u3067\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページのダウンロードとは、そのURLを介してウェブページの内容を取得して処理または保存することです。プログラマは、情報を抽出したり、変更を監視したり、コンテンツをアーカイブしたりするためにこれを行います。これは、ウェブスクレイピング、データマイニング、自動化テストのタスクにおいて基本的な作業です。

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
