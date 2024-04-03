---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:33.691091-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.724760-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\
  \u8FBC\u3080\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\
  \u306B\u4FDD\u5B58\u3055\u308C\u305F\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C7\u30FC\
  \u30BF\u3092\u30A2\u30AF\u30BB\u30B9\u3057\u3066\u53D6\u5F97\u3059\u308B\u3053\u3068\
  \u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3063\u3066\u3001\u5165\u529B\u30C7\u30FC\u30BF\u3001\u8A2D\u5B9A\
  \u3001\u307E\u305F\u306F\u30C7\u30FC\u30BF\u30BB\u30C3\u30C8\u3092\u8AAD\u307F\u8FBC\
  \u3080\u305F\u3081\u306B\u3001\u30B7\u30F3\u30D7\u30EB\u306A\u30B9\u30AF\u30EA\u30D7\
  \u30C8\u304B\u3089\u8907\u96D1\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u307E\u3067\u591A\u304F\u306E\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\
  \u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3068\u306A\u3063\u3066\u3044\u307E\u3059\u3002\
  ."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## 方法:
Dartのコアライブラリ、`dart:io`は、テキストファイルを同期的または非同期的に読み込むために必要な機能を提供しています。両方のアプローチはこちらです。

**同期的に:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // ファイルを同期的に読み込む
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('ファイル読み取りエラー: $e');
  }
}
```

**非同期的に:**

特に大きなファイルや反応性の高いアプリケーションでファイルが読み込まれている間、プログラムがブロックされないようにするために:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('ファイル読み取りエラー: $e');
  }
}
```

**サンプル出力:**

テキストファイルに以下の内容が含まれている場合:

```
Hello, Dart!
```

上記の両方の方法で出力されます:

```
Hello, Dart!
```

**サードパーティのライブラリを使用する:**

ファイル操作の単純化やエラーハンドリングの強化など、追加の機能を求める場合は、`package:file`のようなサードパーティライブラリを検討するかもしれません。しかし、私の最終更新時点では、上記で示したように、コアの`dart:io`パッケージを直接使用することが、Dartでテキストファイルを読み込むための最も一般的で直接的な方法です。
