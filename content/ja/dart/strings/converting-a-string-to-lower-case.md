---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:18.174574-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.284659-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換することは、与えられた文字列の全ての文字をそれぞれの小文字の等価物に変換する基本的な操作です。プログラマーは通常、この操作を実行して大文字と小文字を区別しない比較を行うため、またはさらなる処理のためにテキスト入力を標準化するために行います。これにより、アプリケーションがよりユーザーフレンドリーになり、データがより一貫性を持ちます。

## 方法：

Dartでは、`String` クラスによって提供される `toLowerCase()` メソッドを使用して、文字列を小文字に変換することができます。このメソッドは、全ての大文字を小文字に変換した新しい文字列を返します。これがどのように機能するか、簡単な例で見てみましょう：

```dart
void main() {
  String originalString = "Hello, World!";
  String lowerCaseString = originalString.toLowerCase();

  print(lowerCaseString);  // 出力: hello, world!
}
```

Dartでは、文字列を小文字に変換するなどの基本的な文字列操作タスクに外部ライブラリを要求しません。なぜなら、標準ライブラリの `String` クラスは非常に包括的だからです。しかし、ロケール固有のルールを含むより複雑な操作については、国際化およびローカライゼーション機能を提供する `intl` パッケージを検討することができます。これには、ロケールに基づいた大小文字変換も含まれます：

`intl` を使用するには、それをあなたの `pubspec.yaml` ファイルに追加します：

```yaml
dependencies:
  intl: ^0.17.0
```

次に、特定のロケールに基づいて文字列を小文字に変換するために `toLocaleLowerCase()` メソッドを使用することができます：

```dart
import 'package:intl/intl.dart';

void main() {
  String originalString = "İstanbul";
  
  // トルコ語のロケール
  print(Intl.withLocale('tr', () => originalString.toLowerCase())); // 出力: istanbul
  
  // デフォルトのロケール (en)
  print(originalString.toLowerCase()); // 出力: i̇stanbul
}
```

この例では、トルコ語のロケールがドットなしの 'i' を正しく扱うことを示しており、国際化されたアプリケーションでのロケールに意識した変換の重要性を浮き彫りにしています。
