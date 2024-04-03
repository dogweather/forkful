---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:12.546085-07:00
description: "\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\
  \u5B57\u306B\u5909\u66F4\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u3092\u305D\u306E\
  \u307E\u307E\u306B\u3059\u308B\u3053\u3068\u3092\u3001\u6587\u5B57\u5217\u306E\u5927\
  \u6587\u5B57\u5316\u3068\u8A00\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3084\u30C6\u30AD\u30B9\u30C8\u306E\u8868\u793A\u3092\u6574\u3048\u308B\
  \u305F\u3081\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\
  \u30D5\u30A7\u30A4\u30B9\u306E\u6587\u6CD5\u898F\u5247\u306B\u5F93\u3046\u305F\u3081\
  \u306B\u3001\u3053\u306E\u6280\u8853\u3092\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:41.643567-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\u6587\
  \u5B57\u306B\u5909\u66F4\u3057\u3001\u6B8B\u308A\u306E\u6587\u5B57\u3092\u305D\u306E\
  \u307E\u307E\u306B\u3059\u308B\u3053\u3068\u3092\u3001\u6587\u5B57\u5217\u306E\u5927\
  \u6587\u5B57\u5316\u3068\u8A00\u3044\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u3084\u30C6\u30AD\u30B9\u30C8\u306E\u8868\u793A\u3092\u6574\u3048\u308B\
  \u305F\u3081\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\
  \u30D5\u30A7\u30A4\u30B9\u306E\u6587\u6CD5\u898F\u5247\u306B\u5F93\u3046\u305F\u3081\
  \u306B\u3001\u3053\u306E\u6280\u8853\u3092\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\
  \u3002."
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
