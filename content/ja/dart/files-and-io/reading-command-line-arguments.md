---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:07.745241-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.722303-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## 何となぜ？

Dartでコマンドライン引数を読むことは、プログラマーがDartプログラムを実行する際にコンソールに直接データを入力できるようにし、自動化スクリプト、CLIツール、またはバッチ処理を含む様々な用途に対するそのインタラクティビティと柔軟性を高めます。この機能は、適応性が高くユーザーフレンドリーなコマンドラインアプリケーションを作成するために不可欠です。

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
