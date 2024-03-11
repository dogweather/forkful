---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:38.188910-07:00
description: "Dart\u306E\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\uFF08REPL - Read-Evaluate-Print\u2026"
lastmod: '2024-03-11T00:14:15.306219-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306E\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\uFF08REPL - Read-Evaluate-Print\u2026"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何とは何か？ そして、なぜか？

Dartの対話型シェル（REPL - Read-Evaluate-Print Loop）を利用すると、プログラマーはDartコードを1行ずつ動的に入力し、実行することができ、スクリプト全体をコンパイルする必要がありません。このツールは、Dartの構文を学ぶ、コードスニペットを試す、または即時フィードバックと反復テストを促進することによってデバッグするために、非常に貴重なものです。

## どのようにして：

Dartには組み込みのREPLがありません。しかし、DartPad（オンライン）を使用したり、`dart_repl`のようなサードパーティツールを利用することによって、REPLのような機能を実現することができます。

**DartPadを使用して:**

DartPad（https://dartpad.dev）はオンラインのDartエディタで、ウェブブラウザでDartコードを書いて実行することができます。伝統的なコマンドラインREPLではありませんが、迅速な実験のために同様の経験を提供します。

単にウェブサイトに行き、左側のペインにDartコードを入力し、「Run」をクリックして、右側に出力を表示します。

例：
```dart
void main() {
  print('Hello, Dart!');
}
```
出力：
```
Hello, Dart!
```

**`dart_repl`（サードパーティツール）を使用して：**

まず、pub を通じて `dart_repl` をグローバルにインストールします：

```shell
dart pub global activate dart_repl
```

そして、ターミナルから `dart_repl` を実行します：

```shell
dart_repl
```

これで、シェルに直接Dart文を入力し始めることができます。例えば：

```dart
>>> print('Hello, REPL!');
Hello, REPL!
>>> int add(int x, int y) => x + y;
>>> print(add(5, 7));
12
```

これらの方法は、その場でDartコードを試すための迅速な道を提供し、学習曲線を大幅に緩和し、生産性を向上させます。
