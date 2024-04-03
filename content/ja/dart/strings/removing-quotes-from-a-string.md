---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:24.572302-07:00
description: "\u65B9\u6CD5: Dart\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\
  \u7B26\u3092\u524A\u9664\u3059\u308B\u305F\u3081\u306B\u3001\u30B5\u30FC\u30C9\u30D1\
  \u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\
  \u305A\u306B\u3001\u7D44\u307F\u8FBC\u307F\u306E\u6587\u5B57\u5217\u30E1\u30BD\u30C3\
  \u30C9\u3092\u4F7F\u7528\u3057\u3066\u76F4\u622A\u7684\u306A\u65B9\u6CD5\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:41.664767-06:00'
model: gpt-4-0125-preview
summary: "Dart\u306F\u3001\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\
  \u9664\u3059\u308B\u305F\u3081\u306B\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\
  \u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\
  \u7D44\u307F\u8FBC\u307F\u306E\u6587\u5B57\u5217\u30E1\u30BD\u30C3\u30C9\u3092\u4F7F\
  \u7528\u3057\u3066\u76F4\u622A\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\
  \u3059."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 方法:
Dartは、文字列から引用符を削除するために、サードパーティのライブラリを必要とせずに、組み込みの文字列メソッドを使用して直截的な方法を提供します。

### 例 1: `replaceFirst` と `replaceAll`の使用
文字列が引用符で始まり終わる場合、`replaceFirst` と `replaceAll` メソッドを使用してそれらを除去することができます。

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// ダブルクォーテーションの削除
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // 出力: Hello, World!

// シングルクォーテーションの削除
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // 出力: Dart Programming
```

### 例 2: `substring`の使用
この方法は、引用符が文字列の最初と最後に確実にある場合に便利です。

```dart
String quotedString = '"Flutter Development"';
// エラーを避けるために削除する前に引用符で始まり終わるか確認
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // 出力: Flutter Development
```

### 例 3: カスタム拡張メソッド
特にプロジェクトに引用符の除去が頻繁に含まれる場合には、`String`にカスタム拡張を作成することを検討します。

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // 出力: This is Dart
  print(singleQuoted.unquote()); // 出力: This is awesome
}
```

これらのアプローチは、Dartで効果的に文字列から引用符を削除し、データ処理と準備のワークフローを強化するのに役立ちます。
