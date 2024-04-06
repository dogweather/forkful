---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:49.525854-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306E\u30B3\u30A2\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001\u30D5\u30A1\u30A4\u30EB\u51E6\u7406\u306E\u305F\u3081\u306E`dart:io`\u30D1\
  \u30C3\u30B1\u30FC\u30B8\u3092\u63D0\u4F9B\u3057\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u306A\u3057\u3067\u30C6\u30AD\u30B9\u30C8\
  \u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304F\u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u3092\u66F8\u304F\u7C21\u5358\u306A\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.641976-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法：
Dartのコアライブラリは、ファイル処理のための`dart:io`パッケージを提供し、サードパーティライブラリなしでテキストファイルを書くことを可能にします。以下はテキストファイルを書く簡単な例です：

```dart
import 'dart:io';

void main() async {
  // 現在のディレクトリに'example.txt'という名前の新しいファイルを作成する。
  var file = File('example.txt');
  
  // ファイルに文字列を書き込む。
  await file.writeAsString('Hello, Dart!');
  
  // 内容を確認する。
  print(await file.readAsString()); // 出力：Hello, Dart!
}
```

大きなファイルやデータの流れを扱う場合、`openWrite`を使用して内容を書き込むことを好むかもしれません。これは`IOSink`を返し、データをチャンクで書き込むことを可能にします：

```dart
import 'dart:io';

void main() async {
  var file = File('large_file.txt');
  var sink = file.openWrite();

  // ファイルに複数の行を書き込む。
  sink
    ..writeln('Line 1: The quick brown fox jumps over the lazy dog.')
    ..writeln('Line 2: Dart is awesome!')
    ..close();

  // ファイルにすべてのデータが書き込まれたことを確認するために、シンクの終了を待つ。
  await sink.done;

  // ファイルの内容を読み取って確認するために印刷
  print(await file.readAsString());
}
```

ファイルに追加したり、バイトを書き込んだりするを含む、もっと高度なファイル操作については、`dart:io`によって提供される`File`クラスのメソッドをさらに深く掘り下げることができます。さらに、大規模または複雑なプロジェクトに取り組む際には、ファイルパスを扱うための`path`パッケージやウェブサーバー機能の`shelf`パッケージを検討することが有益かもしれませんが、直接ファイルを書き込む場合は通常、組み込みのDartライブラリに頼ります。
