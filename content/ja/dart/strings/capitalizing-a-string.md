---
title:                "文字列を大文字にする"
date:                  2024-03-08T21:54:12.546085-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

文字列の最初の文字を大文字に変更し、残りの文字をそのままにすることを、文字列の大文字化と言います。プログラマーは、ユーザー入力のフォーマットやテキストの表示を整えるため、またはユーザーインターフェイスの文法規則に従うために、この技術をよく使用します。

## どうやって：

### Dartの組み込みメソッドを使用

Dartでは、文字列操作のためのシンプルで直接的な方法が提供されています。単語や文を大文字化するには、通常、最初の文字を大文字に変換し、それを残りの文字列と連結します。以下はその実装方法です：

```dart
String capitalize(String text) {
  if (text.isEmpty) return text;
  return text[0].toUpperCase() + text.substring(1).toLowerCase();
}

void main() {
  var example = "hello world";
  print(capitalize(example)); // 出力: Hello world
}
```

### 各単語の最初の文字を大文字にする

文字列内の各単語の最初の文字を大文字にするには、文字列を単語に分割し、それぞれを大文字化してから、再び結合します：

```dart
String capitalizeWords(String text) {
  return text.split(' ').map(capitalize).join(' ');
}

void main() {
  var example = "hello dart enthusiasts";
  print(capitalizeWords(example)); // 出力: Hello Dart Enthusiasts
}
```

### サードパーティのライブラリを使用

Dartの標準ライブラリは基本的なニーズをカバーしていますが、特定のタスクはサードパーティのパッケージを使用する方が便利な場合があります。拡張された文字列操作機能、大文字化を含む、[`recase`](https://pub.dev/packages/recase) パッケージは人気のある選択肢です。プロジェクトの `pubspec.yaml` に追加した後、他の機能と同様に簡単に文字列を大文字化できます：

```dart
import 'package:recase/recase.dart';

void main() {
  var example = "hello world";
  var rc = ReCase(example);

  print(rc.titleCase); // 出力: Hello World
}
```

`recase` を使用すると、個々の単語や全ての文を大文字化するだけでなく、他のケーシング規則にも従うことができ、手動で文字列変換を扱うことなく実現できます。
