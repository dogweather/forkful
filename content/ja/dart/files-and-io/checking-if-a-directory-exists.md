---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:56.752140-07:00
description: "Dart\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\
  \u30EB\u30B7\u30B9\u30C6\u30E0\u4E0A\u306E\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\
  \u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u691C\u8A3C\u3057\
  \u3001\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u66F8\u304D\u306A\u3069\u306E\u64CD\
  \u4F5C\u3092\u884C\u3046\u524D\u306B\u5B9F\u65BD\u3055\u308C\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\u30A3\
  \u30EC\u30AF\u30C8\u30EA\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u308A\u5909\u66F4\
  \u3057\u3088\u3046\u3068\u3057\u305F\u3068\u304D\u306B\u767A\u751F\u3059\u308B\u30A8\
  \u30E9\u30FC\u3092\u907F\u3051\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.321803-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\
  \u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\u30EB\
  \u30B7\u30B9\u30C6\u30E0\u4E0A\u306E\u6307\u5B9A\u3055\u308C\u305F\u30D1\u30B9\u3067\
  \u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u691C\u8A3C\u3057\u3001\
  \u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u66F8\u304D\u306A\u3069\u306E\u64CD\u4F5C\
  \u3092\u884C\u3046\u524D\u306B\u5B9F\u65BD\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\u30A3\u30EC\
  \u30AF\u30C8\u30EA\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u308A\u5909\u66F4\u3057\
  \u3088\u3046\u3068\u3057\u305F\u3068\u304D\u306B\u767A\u751F\u3059\u308B\u30A8\u30E9\
  \u30FC\u3092\u907F\u3051\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\
  \u3059\u3002"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u306E\
  \u78BA\u8A8D"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでディレクトリが存在するかを確認することは、ファイルシステム上の指定されたパスでディレクトリの存在を検証し、ファイルの読み書きなどの操作を行う前に実施されます。プログラマーは、存在しないディレクトリにアクセスしたり変更しようとしたときに発生するエラーを避けるためにこれを行います。

## 方法：

Dartは`dart:io`ライブラリを使用してファイルおよびディレクトリを扱います。ディレクトリが存在するかどうかを確認する簡単な方法は以下の通りです：

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('ディレクトリが存在します');
  } else {
    print('ディレクトリが存在しません');
  }
}
```
ディレクトリが存在する場合のサンプル出力：
```
ディレクトリが存在します
```

存在しない場合は：
```
ディレクトリが存在しません
```

もっと複雑なシナリオを処理するには、たとえば非同期で確認したり、存在しない場合にディレクトリを作成したりする場合、以下のアプローチを使用できます：

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // 非同期でディレクトリが存在するか確認
  var exists = await directory.exists();
  if (exists) {
    print('ディレクトリが存在します');
  } else {
    print('ディレクトリが存在しません、作成しています...');
    await directory.create(); // これでディレクトリが作成されます
    print('ディレクトリが作成されました');
  }
}
```

ディレクトリが存在しなくて作成された場合のサンプル出力：
```
ディレクトリが存在しません、作成しています...
ディレクトリが作成されました
```

Dartの組み込み機能は通常、ファイルやディレクトリを扱うには十分であるため、このタスクには通常、サードパーティのライブラリは必要ありません。ただし、より複雑なファイルシステム操作には、`path`（プラットフォームに依存しない方法でパスを操作する）などのパッケージが`dart:io`ライブラリを補完できますが、示されたもの以上のより高度なディレクトリ存在チェックを直接提供するわけではありません。
