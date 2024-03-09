---
title:                "エラーの処理"
date:                  2024-03-08T21:55:56.323341-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

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
