---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:38.188910-07:00
description: "Dart\u306E\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\uFF08REPL - Read-Evaluate-Print\u2026"
lastmod: '2024-03-13T22:44:41.703469-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306E\u5BFE\u8A71\u578B\u30B7\u30A7\u30EB\uFF08REPL - Read-Evaluate-Print\
  \ Loop\uFF09\u3092\u5229\u7528\u3059\u308B\u3068\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306FDart\u30B3\u30FC\u30C9\u30921\u884C\u305A\u3064\u52D5\u7684\u306B\u5165\
  \u529B\u3057\u3001\u5B9F\u884C\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u3001\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u5168\u4F53\u3092\u30B3\u30F3\u30D1\u30A4\u30EB\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308A\u307E\u305B\u3093\u3002\u3053\u306E\u30C4\u30FC\u30EB\
  \u306F\u3001Dart\u306E\u69CB\u6587\u3092\u5B66\u3076\u3001\u30B3\u30FC\u30C9\u30B9\
  \u30CB\u30DA\u30C3\u30C8\u3092\u8A66\u3059\u3001\u307E\u305F\u306F\u5373\u6642\u30D5\
  \u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3068\u53CD\u5FA9\u30C6\u30B9\u30C8\u3092\u4FC3\
  \u9032\u3059\u308B\u3053\u3068\u306B\u3088\u3063\u3066\u30C7\u30D0\u30C3\u30B0\u3059\
  \u308B\u305F\u3081\u306B\u3001\u975E\u5E38\u306B\u8CB4\u91CD\u306A\u3082\u306E\u3067\
  \u3059\u3002."
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
