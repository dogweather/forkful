---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:49.525854-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.725500-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\u304F\
  \u3053\u3068\u306F\u3001\u30C7\u30A3\u30B9\u30AF\u4E0A\u306E\u30D5\u30A1\u30A4\u30EB\
  \u3092\u4F5C\u6210\u307E\u305F\u306F\u5909\u66F4\u3057\u3066\u3001\u8AAD\u307F\u53D6\
  \u308A\u53EF\u80FD\u306A\u5F62\u5F0F\u3067\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u3059\
  \u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u30C7\u30FC\
  \u30BF\u3001\u8A2D\u5B9A\u3001\u30ED\u30B0\u3001\u307E\u305F\u306F\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u5B9F\u884C\u9593\u307E\u305F\u306F\u4ED6\u306E\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3084\u30E6\u30FC\u30B6\u30FC\u3068\
  \u30C7\u30FC\u30BF\u3092\u5171\u6709\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\u60C5\
  \u5831\u3092\u4FDD\u5B58\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 何となぜ？
Dartでテキストファイルを書くことは、ディスク上のファイルを作成または変更して、読み取り可能な形式でデータを保存することを含みます。プログラマーは、アプリケーションのデータ、設定、ログ、またはアプリケーションの実行間または他のアプリケーションやユーザーとデータを共有する必要がある情報を保存するためにこれを行います。

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
