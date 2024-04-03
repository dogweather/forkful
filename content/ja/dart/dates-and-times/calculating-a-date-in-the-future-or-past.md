---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:53:39.281682-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.719406-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u3068\u3063\u3066\u3001\u4E88\
  \u5B9A\u3084\u30EA\u30DE\u30A4\u30F3\u30C0\u30FC\u3001\u307E\u305F\u306F\u65E5\u4ED8\
  \u306E\u8A08\u7B97\u306B\u4F9D\u5B58\u3059\u308B\u6A5F\u80FD\u3092\u6271\u3046\u969B\
  \u3001\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B\u3053\u3068\u306F\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u3067\u3059\
  \u3002\u65E5\u4ED8\u3092\u64CD\u4F5C\u3059\u308B\u65B9\u6CD5\u3092\u7406\u89E3\u3059\
  \u308B\u3053\u3068\u306F\u3001\u30D0\u30C3\u30AF\u30A8\u30F3\u30C9\u30B7\u30B9\u30C6\
  \u30E0\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\
  \u3001\u305D\u3057\u3066\u30C7\u30FC\u30BF\u5206\u6790\u306B\u3068\u3063\u3066\u91CD\
  \u8981\u3067\u3042\u308A\u3001\u7279\u306BDart\u3078\u79FB\u884C\u3057\u3001\u6642\
  \u9593\u8AD6\u7406\u3092\u52B9\u7387\u7684\u306B\u5B9F\u88C5\u3057\u3088\u3046\u3068\
  \u3059\u308B\u4EBA\u3005\u306B\u3068\u3063\u3066\u91CD\u8981\u3067\u3059\u3002."
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
