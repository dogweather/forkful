---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:24.572302-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.664767-06:00'
model: gpt-4-0125-preview
summary: "Dart\u3067\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\
  \u3059\u308B\u3068\u3044\u3046\u306E\u306F\u3001\u6587\u5B57\u5217\u306E\u958B\u59CB\
  \u3068\u7D42\u4E86\u304B\u3089\u30C0\u30D6\u30EB\uFF08\"\uFF09\u307E\u305F\u306F\
  \u30B7\u30F3\u30B0\u30EB\uFF08'\uFF09\u306E\u5F15\u7528\u7B26\u3092\u9664\u53BB\u3059\
  \u308B\u3053\u3068\u3067\u3042\u308A\u3001\u30C7\u30FC\u30BF\u30AF\u30EA\u30FC\u30CB\
  \u30F3\u30B0\u3084\u3055\u3089\u306A\u308B\u51E6\u7406\u306E\u6E96\u5099\u306E\u305F\
  \u3081\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30C7\u30FC\u30BF\u5165\
  \u529B\u306E\u6A19\u6E96\u5316\u3001\u30C7\u30FC\u30BF\u30B9\u30C8\u30EC\u30FC\u30B8\
  \u306E\u7D71\u4E00\u6027\u306E\u4FDD\u8A3C\u3001\u307E\u305F\u306F\u5F15\u7528\u7B26\
  \u5F62\u5F0F\u3067\u30C7\u30FC\u30BF\u3092\u8FD4\u3059\u53EF\u80FD\u6027\u306E\u3042\
  \u308BAPI\u3068\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u6642\u306B\
  \u884C\u3044\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 何となぜ？
Dartで文字列から引用符を削除するというのは、文字列の開始と終了からダブル（"）またはシングル（'）の引用符を除去することであり、データクリーニングやさらなる処理の準備のために役立ちます。プログラマーはこれを行うことで、データ入力の標準化、データストレージの統一性の保証、または引用符形式でデータを返す可能性のあるAPIとのインターフェース時に行います。

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
