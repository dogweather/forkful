---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:44.813597-07:00
description: "Dart\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3068\u306F\u3001\
  \u4E88\u6E2C\u4E0D\u53EF\u80FD\u3067\u5B9F\u884C\u3054\u3068\u306B\u7570\u306A\u308B\
  \u6570\u5024\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30C6\u30B9\u30C8\u74B0\u5883\u3067\u306E\u5B9F\
  \u4E16\u754C\u30B7\u30CA\u30EA\u30AA\u306E\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\
  \u30F3\u304B\u3089\u3001\u30B2\u30FC\u30E0\u30E1\u30AB\u30CB\u30AF\u30B9\u306E\u6709\
  \u52B9\u5316\u3084\u3001\u6697\u53F7\u64CD\u4F5C\u3067\u306E\u30E9\u30F3\u30C0\u30E0\
  \u6027\u3092\u901A\u3058\u305F\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u78BA\u4FDD\
  \u307E\u3067\u3001\u3055\u307E\u3056\u307E\u306A\u7406\u7531\u3067\u3053\u306E\u6A5F\
  \u80FD\u3092\u6D3B\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.693262-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u3068\u306F\u3001\u4E88\
  \u6E2C\u4E0D\u53EF\u80FD\u3067\u5B9F\u884C\u3054\u3068\u306B\u7570\u306A\u308B\u6570\
  \u5024\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30C6\u30B9\u30C8\u74B0\u5883\u3067\u306E\u5B9F\u4E16\
  \u754C\u30B7\u30CA\u30EA\u30AA\u306E\u30B7\u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\
  \u304B\u3089\u3001\u30B2\u30FC\u30E0\u30E1\u30AB\u30CB\u30AF\u30B9\u306E\u6709\u52B9\
  \u5316\u3084\u3001\u6697\u53F7\u64CD\u4F5C\u3067\u306E\u30E9\u30F3\u30C0\u30E0\u6027\
  \u3092\u901A\u3058\u305F\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u78BA\u4FDD\u307E\
  \u3067\u3001\u3055\u307E\u3056\u307E\u306A\u7406\u7531\u3067\u3053\u306E\u6A5F\u80FD\
  \u3092\u6D3B\u7528\u3057\u307E\u3059\u3002."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## どうやって：
Dartのコアライブラリには、`dart:math` にある`Random`クラスを使用して乱数を生成するためのサポートが含まれています。基本的な例を以下に示します：

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // 0 と 99 の間でランダムな整数を生成
  double randomDouble = rand.nextDouble(); // 0.0 と 1.0 の間でランダムなdoubleを生成
  print(randomNumber);
  print(randomDouble);
}
```

*実行例: (実行するたびにこの出力は変わります)*

```
23
0.6722390975465775
```

暗号学的なランダム性を必要とするユースケースのために、Dartは`Random.secure` コンストラクタを提供しています：

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*実行例: (実行するたびにこの出力は変わります)*

```
45
```

Flutterプロジェクトに取り組んでいる場合や、より複雑なランダム性が必要な場合は、名前、住所、日付など、幅広いランダムデータを生成するための`faker`パッケージが便利です。

`faker`を使用するには、まず、`pubspec.yaml`ファイルに追加します：

```yaml
dependencies:
  faker: ^2.0.0
```

次に、示されているようにインポートして使用します：

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // ランダムな名前を生成
  print(faker.address.city()); // ランダムな都市名を生成
}
```

*実行例:*

```
Josie Runolfsdottir
East Lysanne
```
