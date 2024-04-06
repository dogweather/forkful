---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:38.188910-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Dart\u306B\u306F\u7D44\
  \u307F\u8FBC\u307F\u306EREPL\u304C\u3042\u308A\u307E\u305B\u3093\u3002\u3057\u304B\
  \u3057\u3001DartPad\uFF08\u30AA\u30F3\u30E9\u30A4\u30F3\uFF09\u3092\u4F7F\u7528\u3057\
  \u305F\u308A\u3001`dart_repl`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u30C4\u30FC\u30EB\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u306B\u3088\
  \u3063\u3066\u3001REPL\u306E\u3088\u3046\u306A\u6A5F\u80FD\u3092\u5B9F\u73FE\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002 **DartPad\u3092\u4F7F\u7528\
  \u3057\u3066:**\u2026"
lastmod: '2024-04-05T22:37:50.000624-06:00'
model: gpt-4-0125-preview
summary: "**DartPad\u3092\u4F7F\u7528\u3057\u3066:** DartPad\uFF08https://dartpad.dev\uFF09\
  \u306F\u30AA\u30F3\u30E9\u30A4\u30F3\u306EDart\u30A8\u30C7\u30A3\u30BF\u3067\u3001\
  \u30A6\u30A7\u30D6\u30D6\u30E9\u30A6\u30B6\u3067Dart\u30B3\u30FC\u30C9\u3092\u66F8\
  \u3044\u3066\u5B9F\u884C\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u4F1D\u7D71\u7684\u306A\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3REPL\u3067\u306F\
  \u3042\u308A\u307E\u305B\u3093\u304C\u3001\u8FC5\u901F\u306A\u5B9F\u9A13\u306E\u305F\
  \u3081\u306B\u540C\u69D8\u306E\u7D4C\u9A13\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
