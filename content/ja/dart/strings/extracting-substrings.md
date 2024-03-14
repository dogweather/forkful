---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.990059-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u306E\u90E8\u5206\u6587\u5B57\u5217\u306E\
  \u62BD\u51FA\u306F\u3001\u305D\u306E\u4F4D\u7F6E\u3084\u30D1\u30BF\u30FC\u30F3\u306B\
  \u57FA\u3065\u3044\u3066\u7279\u5B9A\u306E\u6587\u5B57\u5217\u306E\u90E8\u5206\u3092\
  \u53D6\u5F97\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u89E3\u6790\u3001\u30C7\
  \u30FC\u30BF\u64CD\u4F5C\u3001\u307E\u305F\u306F\u3088\u308A\u5927\u304D\u306A\u30C6\
  \u30AD\u30B9\u30C8\u30BD\u30FC\u30B9\u304B\u3089\u95A2\u9023\u60C5\u5831\u3092\u62BD\
  \u51FA\u3059\u308B\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306E\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.680269-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u306E\u90E8\u5206\u6587\u5B57\u5217\u306E\
  \u62BD\u51FA\u306F\u3001\u305D\u306E\u4F4D\u7F6E\u3084\u30D1\u30BF\u30FC\u30F3\u306B\
  \u57FA\u3065\u3044\u3066\u7279\u5B9A\u306E\u6587\u5B57\u5217\u306E\u90E8\u5206\u3092\
  \u53D6\u5F97\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u89E3\u6790\u3001\u30C7\
  \u30FC\u30BF\u64CD\u4F5C\u3001\u307E\u305F\u306F\u3088\u308A\u5927\u304D\u306A\u30C6\
  \u30AD\u30B9\u30C8\u30BD\u30FC\u30B9\u304B\u3089\u95A2\u9023\u60C5\u5831\u3092\u62BD\
  \u51FA\u3059\u308B\u306A\u3069\u306E\u30BF\u30B9\u30AF\u306E\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## 何となぜ？
文字列からの部分文字列の抽出は、その位置やパターンに基づいて特定の文字列の部分を取得することです。プログラマーは、ユーザー入力の解析、データ操作、またはより大きなテキストソースから関連情報を抽出するなどのタスクのためにこれを行います。

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
