---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:39.281682-07:00
description: "\u65B9\u6CD5\uFF1A Dart\u306F\u3001\u305D\u306E`DateTime`\u30AF\u30E9\
  \u30B9\u3092\u901A\u3058\u3066\u3001\u65E5\u4ED8\u64CD\u4F5C\u306E\u305F\u3081\u306E\
  \u5805\u7262\u306A\u30B5\u30DD\u30FC\u30C8\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\
  \u3053\u3053\u3067\u306F\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3092\u5FC5\u8981\u3068\u305B\u305A\u306B\u3001\u30CD\u30A4\
  \u30C6\u30A3\u30D6Dart\u3092\u4F7F\u7528\u3057\u3066\u5C06\u6765\u307E\u305F\u306F\
  \u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3059\u308B\u65B9\u6CD5\u3092\u8AAC\
  \u660E\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.635204-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u672A\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u306E\u8A08\u7B97"
weight: 26
---

## 方法：
Dartは、その`DateTime`クラスを通じて、日付操作のための堅牢なサポートを提供します。ここでは、サードパーティのライブラリを必要とせずに、ネイティブDartを使用して将来または過去の日付を計算する方法を説明します。

### 将来の日付を計算する
将来の日付を計算するには、`DateTime`オブジェクトを作成し、希望する期間で`add`メソッドを使用します。

```dart
DateTime today = DateTime.now();
Duration tenDays = Duration(days: 10);
DateTime futureDate = today.add(tenDays);

print(futureDate); // 出力: 2023-04-21 14:22:35.123456 (出力例、現在の日付と時刻に依存します)
```

### 過去の日付を計算する
過去の日付を計算するには、必要な期間で`DateTime`オブジェクトに`subtract`メソッドを使用します。

```dart
DateTime today = DateTime.now();
Duration fifteenDaysAgo = Duration(days: 15);
DateTime pastDate = today.subtract(fifteenDaysAgo);

print(pastDate); // 出力: 2023-03-27 14:22:35.123456 (出力例、現在の日付と時刻に依存します)
```

### サードパーティのライブラリを使用する
Dartのネイティブな日付操作能力は強力ですが、より容易に日付の解析やフォーマットを行ったり、複雑な計算を行うなど、より特定の操作が必要な場合があります。そのような場合、`time`パッケージが非常に便利です。

まず、`pubspec.yaml`の依存関係に`time`を追加します：

```yaml
dependencies:
  time: ^2.0.0
```

次に、それを使用して、より読みやすさを向上させた同様の計算を実行できます：

```dart
import 'package:time/time.dart';

void main() {
  DateTime today = DateTime.now();

  // 将来の日付を計算する
  DateTime futureDate = today + 10.days;
  print(futureDate); // 出力フォーマット: 2023-04-21 14:22:35.123456

  // 過去の日付を計算する
  DateTime pastDate = today - 15.days;
  print(pastDate); // 出力フォーマット: 2023-03-27 14:22:35.123456
}
```

これらの例は、Dartでの基本的な日付操作、現在の日付に対して時間を加算または減算することを含む、Dartアプリケーションでの日付の管理がいかに容易であるかを示しています。
