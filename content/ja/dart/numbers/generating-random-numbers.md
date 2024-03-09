---
title:                "乱数の生成"
date:                  2024-03-08T21:54:44.813597-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
Dartで乱数を生成するとは、予測不可能で実行ごとに異なる数値を作り出すことです。プログラマーは、テスト環境での実世界シナリオのシミュレーションから、ゲームメカニクスの有効化や、暗号操作でのランダム性を通じたセキュリティの確保まで、さまざまな理由でこの機能を活用します。

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
