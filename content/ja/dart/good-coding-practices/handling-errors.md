---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:56.323341-07:00
description: "Dart\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30E0\u5B9F\u884C\u4E2D\u306B\u767A\u751F\u3059\u308B\u4E88\u671F\u305B\
  \u306C\u4F8B\u5916\u3092\u4E88\u6E2C\u3057\u3066\u7BA1\u7406\u3059\u308B\u3053\u3068\
  \u3067\u3001\u4FE1\u983C\u6027\u3068\u4F7F\u3044\u3084\u3059\u3055\u3092\u5411\u4E0A\
  \u3055\u305B\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\
  \u3001\u30E6\u30FC\u30B6\u30FC\u306B\u610F\u5473\u306E\u3042\u308B\u30D5\u30A3\u30FC\
  \u30C9\u30D0\u30C3\u30AF\u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u306B\u30A8\u30E9\
  \u30FC\u51E6\u7406\u3092\u5B9F\u88C5\u3057\u3001\u3088\u308A\u30B9\u30E0\u30FC\u30BA\
  \u3067\u5B89\u5168\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u4F53\u9A13\
  \u3092\u4FDD\u8A3C\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.712710-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u306E\u30A8\u30E9\u30FC\u51E6\u7406\u306F\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u5B9F\u884C\u4E2D\u306B\u767A\u751F\u3059\u308B\u4E88\u671F\u305B\u306C\
  \u4F8B\u5916\u3092\u4E88\u6E2C\u3057\u3066\u7BA1\u7406\u3059\u308B\u3053\u3068\u3067\
  \u3001\u4FE1\u983C\u6027\u3068\u4F7F\u3044\u3084\u3059\u3055\u3092\u5411\u4E0A\u3055\
  \u305B\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30AF\u30E9\u30C3\u30B7\u30E5\u3092\u9632\u304E\u3001\
  \u30E6\u30FC\u30B6\u30FC\u306B\u610F\u5473\u306E\u3042\u308B\u30D5\u30A3\u30FC\u30C9\
  \u30D0\u30C3\u30AF\u3092\u63D0\u4F9B\u3059\u308B\u305F\u3081\u306B\u30A8\u30E9\u30FC\
  \u51E6\u7406\u3092\u5B9F\u88C5\u3057\u3001\u3088\u308A\u30B9\u30E0\u30FC\u30BA\u3067\
  \u5B89\u5168\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u4F53\u9A13\u3092\
  \u4FDD\u8A3C\u3057\u307E\u3059\u3002"
title: "\u30A8\u30E9\u30FC\u306E\u51E6\u7406"
weight: 16
---

## 何となぜ？
Dartでのエラー処理は、プログラム実行中に発生する予期せぬ例外を予測して管理することで、信頼性と使いやすさを向上させることについてです。プログラマーは、クラッシュを防ぎ、ユーザーに意味のあるフィードバックを提供するためにエラー処理を実装し、よりスムーズで安全なアプリケーション体験を保証します。

## どのようにして：
Dartは、*コンパイル時* エラーと *実行時* エラーの2種類のエラーをサポートしています。コンパイル時エラーはコードが実行される前にDart解析器によって検出されますが、実行時エラーまたは例外は実行中に発生します。Dartで例外を処理する方法は次のとおりです：

### Try-Catch
アプリケーションがクラッシュするのを防ぐために、`try-catch`を使用して例外を捕捉します：

```dart
try {
  var result = 100 ~/ 0; // 0での割り算を試みると、例外がスローされる
} catch (e) {
  print('例外を捕捉しました: $e'); // 例外を処理する
}
```
出力例: `例外を捕捉しました: IntegerDivisionByZeroException`

### 特定の例外
特定の例外を処理するには、`catch`の後に例外を記述します：

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('0で割ることはできません。'); // 0での割り算例外を特別に処理します
}
```
出力例: `0で割ることはできません。`

### スタックトレース
デバッグ用のスタックトレースを取得するには、catchブロックに二番目のパラメータを使用します：

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('例外: $e');
  print('スタックトレース: $s'); // デバッグのためのスタックトレースを印刷する
}
```

### Finally
例外がスローされたかどうかに関わらず、try/catchの後にコードを実行するには、`finally`を使用します：

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('例外を捕捉しました: $e');
} finally {
  print('これは常に実行されます。'); // クリーンアップコードや最終ステップ
}
```
出力例:
```
例外を捕捉しました: IntegerDivisionByZeroException
これは常に実行されます。
```

### サードパーティライブラリ
Dartのコアライブラリはエラー処理に対して堅牢ですが、`dartz`のようなサードパーティパッケージを使用して、`Either`や`Option`などの概念を導入し、エラー処理に使用することもできます。これは、エラー処理に`dartz`を使用する例です：

1. 依存関係の下に`dartz`を`pubspec.yaml`ファイルに追加してください：
```yaml
dependencies:
  dartz: ^0.10.0
```

2. Dartコード内でエラーをうまく処理するために`Either`を使用します：
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('0で割ることはできません。');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('エラー: $left'), 
    (right) => print('結果: $right')
  );
}
```
出力例: `エラー: 0で割ることはできません。`

`Left`部分は通常、エラーを表し、`Right`部分は成功を表します。このパターンは、エラーをより機能的な方法で処理することを可能にし、エラー管理における明確性と制御を提供します。
