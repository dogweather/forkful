---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:12.546085-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Dart\u3067\u306F\u3001\u6587\u5B57\
  \u5217\u64CD\u4F5C\u306E\u305F\u3081\u306E\u30B7\u30F3\u30D7\u30EB\u3067\u76F4\u63A5\
  \u7684\u306A\u65B9\u6CD5\u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\
  \u5358\u8A9E\u3084\u6587\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u306B\u306F\u3001\
  \u901A\u5E38\u3001\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\
  \u63DB\u3057\u3001\u305D\u308C\u3092\u6B8B\u308A\u306E\u6587\u5B57\u5217\u3068\u9023\
  \u7D50\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u5B9F\u88C5\u65B9\u6CD5\
  \u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.975350-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A Dart\u3067\u306F\u3001\u6587\u5B57\u5217\
  \u64CD\u4F5C\u306E\u305F\u3081\u306E\u30B7\u30F3\u30D7\u30EB\u3067\u76F4\u63A5\u7684\
  \u306A\u65B9\u6CD5\u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u5358\
  \u8A9E\u3084\u6587\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u306B\u306F\u3001\u901A\
  \u5E38\u3001\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\u5B57\u306B\u5909\u63DB\
  \u3057\u3001\u305D\u308C\u3092\u6B8B\u308A\u306E\u6587\u5B57\u5217\u3068\u9023\u7D50\
  \u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u5B9F\u88C5\u65B9\u6CD5\u3067\
  \u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## どうやって：


### Dartの組み込みメソッドを使用
Dartでは、文字列操作のためのシンプルで直接的な方法が提供されています。単語や文を大文字化するには、通常、最初の文字を大文字に変換し、それを残りの文字列と連結します。以下はその実装方法です：

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // 出力: Hello world
}
```

### 各単語の最初の文字を大文字にする
文字列内の各単語の最初の文字を大文字にするには、文字列を単語に分割し、それぞれを大文字化してから、再び結合します：

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // 出力: Hello Dart Enthusiasts
}
```

### サードパーティのライブラリを使用
Dartの標準ライブラリは基本的なニーズをカバーしていますが、特定のタスクはサードパーティのパッケージを使用する方が便利な場合があります。拡張された文字列操作機能、大文字化を含む、[`recase`](https://pub.dev/packages/recase) パッケージは人気のある選択肢です。プロジェクトの `pubspec.yaml` に追加した後、他の機能と同様に簡単に文字列を大文字化できます：

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // 出力: Hello World
}
```

`recase` を使用すると、個々の単語や全ての文を大文字化するだけでなく、他のケーシング規則にも従うことができ、手動で文字列変換を扱うことなく実現できます。
