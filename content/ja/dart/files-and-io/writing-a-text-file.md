---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:49.525854-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.725500-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
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
