---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:44.813597-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Dart\u306E\u30B3\u30A2\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306B\u306F\u3001`dart:math` \u306B\u3042\u308B`Random`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\
  \u305F\u3081\u306E\u30B5\u30DD\u30FC\u30C8\u304C\u542B\u307E\u308C\u3066\u3044\u307E\
  \u3059\u3002\u57FA\u672C\u7684\u306A\u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\
  \u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.991962-06:00'
model: gpt-4-0125-preview
summary: "math` \u306B\u3042\u308B`Random`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\
  \u3066\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u305F\u3081\u306E\u30B5\u30DD\u30FC\
  \u30C8\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\u3002\u57FA\u672C\u7684\u306A\
  \u4F8B\u3092\u4EE5\u4E0B\u306B\u793A\u3057\u307E\u3059\uFF1A."
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
