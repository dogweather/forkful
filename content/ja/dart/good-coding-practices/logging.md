---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:24.021996-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Dart\u306B\u306F\u3001`dart:developer`\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u901A\u3058\u305F\u30B7\u30F3\u30D7\u30EB\u306A\u30ED\
  \u30B0\u53D6\u308A\u30E1\u30AB\u30CB\u30BA\u30E0\u304C\u542B\u307E\u308C\u3066\u3044\
  \u307E\u3059\u3002\u3088\u308A\u6D17\u7DF4\u3055\u308C\u305F\u30ED\u30B0\u53D6\u308A\
  \u306E\u30CB\u30FC\u30BA\u306B\u5BFE\u3057\u3066\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3057\u3070\u3057\u3070`logger`\u3084`log4dart`\u306E\u3088\u3046\u306A\
  \u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\
  \u983C\u308A\u307E\u3059\u3002\u2026"
lastmod: '2024-03-13T22:44:41.710746-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306B\u306F\u3001`dart:developer`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\
  \u901A\u3058\u305F\u30B7\u30F3\u30D7\u30EB\u306A\u30ED\u30B0\u53D6\u308A\u30E1\u30AB\
  \u30CB\u30BA\u30E0\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002\u3088\u308A\
  \u6D17\u7DF4\u3055\u308C\u305F\u30ED\u30B0\u53D6\u308A\u306E\u30CB\u30FC\u30BA\u306B\
  \u5BFE\u3057\u3066\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3057\u3070\u3057\
  \u3070`logger`\u3084`log4dart`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u983C\u308A\u307E\u3059."
title: "\u30ED\u30B0\u8A18\u9332"
weight: 17
---

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
