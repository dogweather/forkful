---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.990059-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u3067\u306F\u3001`substring()`\u3001`split()`\u3001\
  \u6B63\u898F\u8868\u73FE\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u65B9\u6CD5\
  \u3067\u90E8\u5206\u6587\u5B57\u5217\u3092\u62BD\u51FA\u3059\u308B\u3053\u3068\u304C\
  \u3067\u304D\u307E\u3059\u3002\u5404\u65B9\u6CD5\u306F\u7570\u306A\u308B\u76EE\u7684\
  \u3092\u6301\u3061\u3001\u6587\u5B57\u5217\u306E\u6271\u3044\u306B\u304A\u3044\u3066\
  \u67D4\u8EDF\u6027\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.598930-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA"
weight: 6
---

## 方法：
Dartでは、`substring()`、`split()`、正規表現など、さまざまな方法で部分文字列を抽出することができます。各方法は異なる目的を持ち、文字列の扱いにおいて柔軟性を提供します。

### `substring()` の使用：
`substring()` メソッドはわかりやすいです。開始インデックス（オプションで終了インデックスも）を指定して文字列をスライスします。

```dart
void main() {
  String example = "Hello, World!";
  String result = example.substring(7, 12);
  print(result); // 出力: World
}
```

### `split()` の使用：
パターン（スペースやカンマなど）に基づいて文字列を部分文字列のリストに分割し、その後インデックスで部分文字列にアクセスします。

```dart
void main() {
  String example = "Dart is fun";
  List<String> parts = example.split(' ');
  String result = parts[1]; // インデックスでアクセス
  print(result); // 出力: is
}
```

### 正規表現の使用：
複雑なパターンには、Dartの`RegExp`クラスが強力です。パターンにマッチし、部分文字列を抽出するために使用します。

```dart
void main() {
  String example = "Email: example@mail.com";
  RegExp regExp = RegExp(r"\b\w+@\w+\.\w+\b");
  String email = regExp.stringMatch(example)!;
  print(email); // 出力: example@mail.com
}
```

### サードパーティのライブラリ：
Dartの標準ライブラリはかなり能力が高いものの、サードパーティのライブラリがタスクを簡単にするシナリオに遭遇するかもしれません。文字列操作とパターンマッチングに関しては、特にここで推奨されるものはありませんが、Dartの組み込み機能だけで十分であることが多いです。しかし、特定のニーズにより適したライブラリが[pub.dev](https://pub.dev)で見つかるかもしれないので、常にチェックしてください。
