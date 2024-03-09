---
title:                "標準エラーへの書き込み"
date:                  2024-03-08T21:58:31.101907-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
