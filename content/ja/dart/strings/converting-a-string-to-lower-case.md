---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:18.174574-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u3067\u306F\u3001`String` \u30AF\u30E9\u30B9\
  \u306B\u3088\u3063\u3066\u63D0\u4F9B\u3055\u308C\u308B `toLowerCase()` \u30E1\u30BD\
  \u30C3\u30C9\u3092\u4F7F\u7528\u3057\u3066\u3001\u6587\u5B57\u5217\u3092\u5C0F\u6587\
  \u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  \u3053\u306E\u30E1\u30BD\u30C3\u30C9\u306F\u3001\u5168\u3066\u306E\u5927\u6587\u5B57\
  \u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3057\u305F\u65B0\u3057\u3044\u6587\u5B57\
  \u5217\u3092\u8FD4\u3057\u307E\u3059\u3002\u3053\u308C\u304C\u3069\u306E\u3088\u3046\
  \u306B\u6A5F\u80FD\u3059\u308B\u304B\u3001\u7C21\u5358\u306A\u4F8B\u3067\u898B\u3066\
  \u307F\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-04-05T22:37:49.980818-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Dart\u3067\u306F\u3001`String` \u30AF\u30E9\u30B9\u306B\
  \u3088\u3063\u3066\u63D0\u4F9B\u3055\u308C\u308B `toLowerCase()` \u30E1\u30BD\u30C3\
  \u30C9\u3092\u4F7F\u7528\u3057\u3066\u3001\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\
  \u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u306E\u30E1\u30BD\u30C3\u30C9\u306F\u3001\u5168\u3066\u306E\u5927\u6587\u5B57\u3092\
  \u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3057\u305F\u65B0\u3057\u3044\u6587\u5B57\u5217\
  \u3092\u8FD4\u3057\u307E\u3059\u3002\u3053\u308C\u304C\u3069\u306E\u3088\u3046\u306B\
  \u6A5F\u80FD\u3059\u308B\u304B\u3001\u7C21\u5358\u306A\u4F8B\u3067\u898B\u3066\u307F\
  \u307E\u3057\u3087\u3046\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B"
weight: 4
---

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
