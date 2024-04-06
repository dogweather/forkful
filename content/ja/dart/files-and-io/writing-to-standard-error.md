---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:31.101907-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u3067stderr\u306B\u66F8\u304D\u8FBC\u3080\u306E\
  \u306F\u3001`dart:io`\u3067\u5229\u7528\u53EF\u80FD\u306A`stderr`\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u76F4\u63A5\u7684\
  \u3067\u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u3053\u3061\u3089\u306B\u793A\
  \u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.639187-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6A19\u6E96\u30A8\u30E9\u30FC\u3078\u306E\u66F8\u304D\u8FBC\u307F"
weight: 25
---

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
