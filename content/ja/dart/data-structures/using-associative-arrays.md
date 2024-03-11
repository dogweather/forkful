---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:57.005810-07:00
description: "Dart\u3067\u306E\u9023\u60F3\u914D\u5217\u3001\u4E00\u822C\u306B\u30DE\
  \u30C3\u30D7\u3068\u3057\u3066\u77E5\u3089\u308C\u3066\u3044\u308B\u3082\u306E\u306F\
  \u3001\u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u3067\u30C7\u30FC\u30BF\u3092\u683C\
  \u7D0D\u3059\u308B\u30C7\u30FC\u30BF\u69CB\u9020\u3067\u3059\u3002\u3053\u308C\u3089\
  \u306F\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u8981\u7D20\u306B\u30A4\u30F3\u30C7\
  \u30C3\u30AF\u30B9\u3067\u306F\u306A\u304F\u3001\u30AD\u30FC\u3092\u901A\u3057\u3066\
  \u30A2\u30AF\u30BB\u30B9\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3053\u3068\
  \u3067\u3001\u5404\u8981\u7D20\u306B\u4E00\u610F\u306E\u8B58\u5225\u5B50\u304C\u3042\
  \u308B\u69CB\u9020\u5316\u3055\u308C\u305F\u30C7\u30FC\u30BF\u3092\u6271\u3046\u969B\
  \u306B\u3001\u30C7\u30FC\u30BF\u306E\u53D6\u5F97\u3092\u76F4\u611F\u7684\u304B\u3064\
  \u52B9\u7387\u7684\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.292662-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306E\u9023\u60F3\u914D\u5217\u3001\u4E00\u822C\u306B\u30DE\u30C3\
  \u30D7\u3068\u3057\u3066\u77E5\u3089\u308C\u3066\u3044\u308B\u3082\u306E\u306F\u3001\
  \u30AD\u30FC\u3068\u5024\u306E\u30DA\u30A2\u3067\u30C7\u30FC\u30BF\u3092\u683C\u7D0D\
  \u3059\u308B\u30C7\u30FC\u30BF\u69CB\u9020\u3067\u3059\u3002\u3053\u308C\u3089\u306F\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u8981\u7D20\u306B\u30A4\u30F3\u30C7\u30C3\
  \u30AF\u30B9\u3067\u306F\u306A\u304F\u3001\u30AD\u30FC\u3092\u901A\u3057\u3066\u30A2\
  \u30AF\u30BB\u30B9\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3053\u3068\u3067\
  \u3001\u5404\u8981\u7D20\u306B\u4E00\u610F\u306E\u8B58\u5225\u5B50\u304C\u3042\u308B\
  \u69CB\u9020\u5316\u3055\u308C\u305F\u30C7\u30FC\u30BF\u3092\u6271\u3046\u969B\u306B\
  \u3001\u30C7\u30FC\u30BF\u306E\u53D6\u5F97\u3092\u76F4\u611F\u7684\u304B\u3064\u52B9\
  \u7387\u7684\u306B\u3057\u307E\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となく理由は？

Dartでの連想配列、一般にマップとして知られているものは、キーと値のペアでデータを格納するデータ構造です。これらはプログラマーが要素にインデックスではなく、キーを通してアクセスできるようにすることで、各要素に一意の識別子がある構造化されたデータを扱う際に、データの取得を直感的かつ効率的にします。

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
