---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:57.005810-07:00
description: "\u65B9\u6CD5: Dart\u306F\u30DE\u30C3\u30D7\u3092\u4F5C\u6210\u304A\u3088\
  \u3073\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u76F4\u611F\u7684\u306A\u69CB\u6587\
  \u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u4F5C\
  \u6210\u3001\u8981\u7D20\u306E\u8FFD\u52A0\u3001\u5024\u306E\u53D6\u5F97\u3068\u3044\
  \u3063\u305F\u57FA\u672C\u64CD\u4F5C\u3092\u793A\u3059\u4F8B\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.687475-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F\u30DE\u30C3\u30D7\u3092\u4F5C\u6210\u304A\u3088\u3073\u64CD\u4F5C\
  \u3059\u308B\u305F\u3081\u306E\u76F4\u611F\u7684\u306A\u69CB\u6587\u3092\u63D0\u4F9B\
  \u3057\u3066\u3044\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u4F5C\u6210\u3001\u8981\
  \u7D20\u306E\u8FFD\u52A0\u3001\u5024\u306E\u53D6\u5F97\u3068\u3044\u3063\u305F\u57FA\
  \u672C\u64CD\u4F5C\u3092\u793A\u3059\u4F8B\u3067\u3059."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法:
Dartはマップを作成および操作するための直感的な構文を提供しています。以下は、作成、要素の追加、値の取得といった基本操作を示す例です。

```dart
void main() {
  // マップの作成
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple'
  };

  // 新しいキーと値のペアを追加
  fruitColors['orange'] = 'orange';

  // キーによって値にアクセス
  print(fruitColors['apple']); // 出力: red

  // 値の更新
  fruitColors['banana'] = 'green';

  // マップをイテレート
  fruitColors.forEach((fruit, color) {
    print('$fruit: $color');
  });
  // サンプル出力:
  // apple: red
  // banana: green
  // grape: purple
  // orange: orange
}
```

複雑なデータ構造や拡張機能について、Dartのプログラマーはよく追加のライブラリに依存しています。そのようなライブラリの一つが `collection` で、高度なコレクションタイプとユーティリティを提供します。`collection` は基本的なマップの取り扱いを変更するわけではありませんが、ユーティリティ関数やより洗練されたコレクションタイプでそれを豊かにします。値によってマップをソートするなど、より具体的なタスクに使用する方法は次のとおりです：

まず、`pubspec.yaml` ファイルに `collection` パッケージが含まれていることを確認します：

```yaml
dependencies:
  collection: ^1.15.0
```

その後、次のように使用できます：

```dart
import 'package:collection/collection.dart';

void main() {
  var fruitColors = {
    'apple': 'red',
    'banana': 'yellow',
    'grape': 'purple',
    'orange': 'orange'
  };

  // 値（色）によってマップをソート
  var sortedFruitsByColor = SplayTreeMap.from(
    fruitColors,
    (key1, key2) => fruitColors[key1]!.compareTo(fruitColors[key2]!)
  );

  print(sortedFruitsByColor);
  // 出力:
  // {orange: orange, apple: red, banana: yellow, grape: purple}
}
```

この例は、値に基づいてマップのエントリをソートする方法を示しており、Dartとその活気あるエコシステムが連想配列をより洗練されたデータ操作にどのように柔軟に扱うことができるかを示しています。
