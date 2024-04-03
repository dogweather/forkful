---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:07.745241-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.722303-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\
  \u3080\u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304CDart\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u3092\u5B9F\u884C\u3059\u308B\u969B\u306B\u30B3\u30F3\u30BD\
  \u30FC\u30EB\u306B\u76F4\u63A5\u30C7\u30FC\u30BF\u3092\u5165\u529B\u3067\u304D\u308B\
  \u3088\u3046\u306B\u3057\u3001\u81EA\u52D5\u5316\u30B9\u30AF\u30EA\u30D7\u30C8\u3001\
  CLI\u30C4\u30FC\u30EB\u3001\u307E\u305F\u306F\u30D0\u30C3\u30C1\u51E6\u7406\u3092\
  \u542B\u3080\u69D8\u3005\u306A\u7528\u9014\u306B\u5BFE\u3059\u308B\u305D\u306E\u30A4\
  \u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D3\u30C6\u30A3\u3068\u67D4\u8EDF\u6027\u3092\
  \u9AD8\u3081\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001\u9069\u5FDC\u6027\
  \u304C\u9AD8\u304F\u30E6\u30FC\u30B6\u30FC\u30D5\u30EC\u30F3\u30C9\u30EA\u30FC\u306A\
  \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\
  \u30F3\u3092\u4F5C\u6210\u3059\u308B\u305F\u3081\u306B\u4E0D\u53EF\u6B20\u3067\u3059\
  \u3002."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

## 方法:
Dartは`List<String> args` をmainメソッドで使用してコマンドライン引数にアクセスするための単純なアプローチを提供します。以下は、コマンドライン引数を読み取り、利用する方法を示すシンプルな例です。

```dart
// main.dart
void main(List<String> args) {
  print('コマンドライン引数:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

このDartプログラムを実行してコマンドライン引数を渡すには、以下のようにDart CLIを使用します：

```shell
dart run main.dart Hello World!
```

期待される出力：

```
コマンドライン引数:
1: Hello
2: World!
```

### 人気のあるサードパーティーライブラリ `args` の使用
Dartの組み込み機能は多くのアプリケーションに対して堅牢ですが、`args`パッケージはより複雑なニーズに対してコマンドライン引数を定義し解析する洗練された方法を提供します。

まず、`pubspec.yaml`に`args`パッケージを追加します：

```yaml
dependencies:
  args: ^2.0.0
```

その後、以下のようにプログラムで使用します：

```dart
// 'args' パッケージを使用
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('こんにちは、${argResults['name']}！');
  } else {
    print('名前が提供されていません。');
  }
}
```

名前付き引数でプログラムを実行する：

```shell
dart run main.dart --name=John
```

期待される出力：

```
こんにちは、John！
```

このコマンドライン引数を解析するシンプルな導入は、ネイティブおよび`args`ライブラリを使用したものであり、Dartがコンソールから直接ユーザー入力を処理する方法を示しており、よりインタラクティブでダイナミックなCLIアプリケーションの作成への道を開きます。
