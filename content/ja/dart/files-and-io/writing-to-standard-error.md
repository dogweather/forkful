---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:31.101907-07:00
description: "Dart\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\
  \u306E\u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\
  \u30B8\u3084\u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\
  \u7570\u306A\u308B\u5225\u306E\u30B9\u30C8\u30EA\u30FC\u30E0\u306B\u9001\u4FE1\u3059\
  \u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u6B63\u5E38\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\
  \u30A8\u30E9\u30FC\u3084\u8B66\u544A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\
  \u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30ED\u30B0\u306E\u53D6\
  \u5F97\u304C\u5BB9\u6613\u306B\u306A\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.723781-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306E\u6A19\u6E96\u30A8\u30E9\u30FC\uFF08stderr\uFF09\u3078\u306E\
  \u66F8\u304D\u8FBC\u307F\u306F\u3001\u30A8\u30E9\u30FC\u30E1\u30C3\u30BB\u30FC\u30B8\
  \u3084\u8A3A\u65AD\u3092\u6A19\u6E96\u51FA\u529B\uFF08stdout\uFF09\u3068\u306F\u7570\
  \u306A\u308B\u5225\u306E\u30B9\u30C8\u30EA\u30FC\u30E0\u306B\u9001\u4FE1\u3059\u308B\
  \u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u6B63\u5E38\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u51FA\u529B\u3068\u30A8\
  \u30E9\u30FC\u3084\u8B66\u544A\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u533A\u5225\u3059\
  \u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u30C7\u30D0\u30C3\u30B0\u3084\u30ED\u30B0\u306E\u53D6\u5F97\
  \u304C\u5BB9\u6613\u306B\u306A\u308A\u307E\u3059\u3002."
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

## 何となぜ？

Dartでの標準エラー（stderr）への書き込みは、エラーメッセージや診断を標準出力（stdout）とは異なる別のストリームに送信することを指します。プログラマーは、正常なプログラム出力とエラーや警告メッセージを区別するためにこれを行います。これにより、デバッグやログの取得が容易になります。

## 方法：

Dartでstderrに書き込むのは、`dart:io`で利用可能な`stderr`オブジェクトを使用することで直接的です。基本的な例をこちらに示します：

```dart
import 'dart:io';

void main() {
  stderr.writeln('これはエラーメッセージです。');
}
```

実行時の出力：
```
これはエラーメッセージです。
```
このメッセージはstderrストリームに送信され、通常はコンソールやターミナルに表示されます。

例外をログに記録するなど、より複雑なデモンストレーションを行うには、Dartの豊富な機能セットが簡潔で効果的なエラー処理を可能にします：

```dart
import 'dart:io';

void riskyOperation() {
  try {
    // 例外を投げる可能性のある操作をシミュレート
    throw Exception('問題が発生しました！');
  } catch (e) {
    stderr.writeln('エラー：$e');
  }
}

void main() {
  riskyOperation();
}
```

実行時の出力：
```
エラー：例外：問題が発生しました！
```

このパターンは、通常のログとエラーログを分離する必要があるアプリケーションに特に有用であり、アプリケーションの監視とデバッグを容易にします。

Dartの標準ライブラリはかなり包括的ですが、多くのプログラムではstderrへの書き込みにサードパーティのライブラリを必要としません。ただし、アプリケーションにより高度なロギング機能（例：ファイルへの記録、ネットワーク経由、フォーマット）が必要な場合は、`logging`パッケージが人気の選択肢です。エラー用に`logging`を使用する際の簡単な例を以下に示します：

```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('重大なエラー：かなり悪いことが起こりました。');
}
```

実行時の出力：
```
SEVERE: 2023-04-01 00:00:00.000: 重大なエラー：かなり悪いことが起こりました。
```

この方法は、エラーとして何がログに記録され、その形式がどのようになるかに関して、より高度なカスタマイズと制御を提供するため、より大きく複雑なアプリケーションで非常に役立ちます。
