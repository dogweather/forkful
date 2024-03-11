---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:24.021996-07:00
description: "Dart\u3067\u306E\u30ED\u30B0\u53D6\u308A\u306F\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306E\u5B9F\u884C\u4E2D\u306B\u69D8\u3005\u306A\u30EC\u30D9\u30EB\u306E\
  \u60C5\u5831\u3092\u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3092\u6307\u3057\
  \u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30BD\u30D5\u30C8\
  \u30A6\u30A7\u30A2\u306E\u52D5\u4F5C\u3092\u76E3\u8996\u3057\u3001\u554F\u984C\u3092\
  \u30C7\u30D0\u30C3\u30B0\u3057\u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\
  \u5206\u6790\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u3001\u6642\
  \u9593\u306E\u7D4C\u904E\u3068\u3068\u3082\u306B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306E\u4FDD\u5B88\u3068\u6539\u5584\u3092\u5BB9\u6613\u306B\u3057\u307E\
  \u3059\u3002"
lastmod: '2024-03-11T00:14:15.312749-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306E\u30ED\u30B0\u53D6\u308A\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u306E\u5B9F\u884C\u4E2D\u306B\u69D8\u3005\u306A\u30EC\u30D9\u30EB\u306E\u60C5\
  \u5831\u3092\u8A18\u9332\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3092\u6307\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30BD\u30D5\u30C8\u30A6\
  \u30A7\u30A2\u306E\u52D5\u4F5C\u3092\u76E3\u8996\u3057\u3001\u554F\u984C\u3092\u30C7\
  \u30D0\u30C3\u30B0\u3057\u3001\u30D1\u30D5\u30A9\u30FC\u30DE\u30F3\u30B9\u3092\u5206\
  \u6790\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u3001\u6642\u9593\
  \u306E\u7D4C\u904E\u3068\u3068\u3082\u306B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u306E\u4FDD\u5B88\u3068\u6539\u5584\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\
  \u3002"
title: "\u30ED\u30B0\u8A18\u9332"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでのログ取りは、プログラムの実行中に様々なレベルの情報を記録するプロセスを指します。プログラマーは、ソフトウェアの動作を監視し、問題をデバッグし、パフォーマンスを分析するためにこれを行い、時間の経過とともにアプリケーションの保守と改善を容易にします。

## どうやって：

Dartには、`dart:developer`ライブラリを通じたシンプルなログ取りメカニズムが含まれています。より洗練されたログ取りのニーズに対して、プログラマーはしばしば`logger`や`log4dart`のようなサードパーティのライブラリに頼ります。

### `dart:developer`を使用する
これは、開発中に特に基本的なログ取りに適しています：

```dart
import 'dart:developer';

void main() {
  log('これはデバッグログメッセージです。');
}
```

出力：
```
これはデバッグログメッセージです。
```

### `logger`パッケージを使用する
より包括的なソリューションとして、`logger`パッケージは様々なレベルのログ取り（例：情報、警告、エラー）を提供し、より読みやすい形式でフォーマットすることができます。

まず、`pubspec.yaml`ファイルに`logger`依存関係を追加します：

```yaml
dependencies:
  logger: ^1.0.0
```

次に、以下のように使用します：

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("これはデバッグメッセージです");
  logger.w("これは警告メッセージです");
  logger.e("これはエラーメッセージです");
}
```

サンプル出力は、各メッセージタイプが簡単に識別できるように異なる形式で表示されることがあります：

```
💬 これはデバッグメッセージです
⚠️ これは警告メッセージです
❗️ これはエラーメッセージです
```

### `log4dart`パッケージを使用する
Log4jに似た設定ベースのログ取りが必要なアプリケーションに対して、`log4dart`は馴染みやすいアプローチを提供します。これは特に大規模なアプリケーションに便利です。

`pubspec.yaml`に`log4dart`を含めることを確認します：

```yaml
dependencies:
  log4dart: ^2.0.0
```

簡単な使用例：

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("MyAppをデバッグ中");
  logger.info("情報メッセージ");
}
```

出力：

```
DEBUG: MyAppをデバッグ中
INFO: 情報メッセージ
```

これらの方法はそれぞれ、シンプルなデバッグメッセージから複雑なアプリケーションのニーズに合った包括的で設定可能なログ取りに至るまで、異なるレベルの柔軟性と複雑さを提供します。
