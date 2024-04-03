---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:04.111937-07:00
description: "Java\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u3053\u3068\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\u65E5\u4ED8\u8A08\u7B97\u3001\
  \u6642\u9593\u30D9\u30FC\u30B9\u306E\u6761\u4EF6\u306A\u3069\u306E\u64CD\u4F5C\u306B\
  \u65E5\u4ED8\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u64CD\u4F5C\u3059\u308B\u3053\
  \u3068\u3092\u53EF\u80FD\u306B\u3059\u308B\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3067\
  \u3059\u3002\u8FFD\u8DE1\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\
  \u304A\u3088\u3073\u6642\u9593\u7684\u30C7\u30FC\u30BF\u5206\u6790\u304C\u91CD\u8981\
  \u3067\u3042\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306F\u4E0D\
  \u53EF\u6B20\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.962414-06:00'
model: gpt-4-0125-preview
summary: "Java\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\u65E5\u4ED8\u8A08\u7B97\u3001\u6642\
  \u9593\u30D9\u30FC\u30B9\u306E\u6761\u4EF6\u306A\u3069\u306E\u64CD\u4F5C\u306B\u65E5\
  \u4ED8\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u64CD\u4F5C\u3059\u308B\u3053\u3068\
  \u3092\u53EF\u80FD\u306B\u3059\u308B\u57FA\u672C\u7684\u306A\u64CD\u4F5C\u3067\u3059\
  \u3002\u8FFD\u8DE1\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u3001\u304A\
  \u3088\u3073\u6642\u9593\u7684\u30C7\u30FC\u30BF\u5206\u6790\u304C\u91CD\u8981\u3067\
  \u3042\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u306F\u4E0D\u53EF\
  \u6B20\u3067\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？
Javaで現在の日付を取得することは、ログ記録、日付計算、時間ベースの条件などの操作に日付オブジェクトを操作することを可能にする基本的な操作です。追跡、スケジューリング、および時間的データ分析が重要であるアプリケーションでは不可欠です。

## 方法：
Javaでは、古い `java.util.Date` クラスと、より多様で直感的な新しい `java.time` パッケージ（Java 8で導入）の両方を使用して、現在の日付を取得する複数の方法があります。

### `java.time.LocalDate` を使用する
```java
import java.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // 例出力: 2023-04-01
    }
}
```
### `java.time.LocalDateTime` を使用する
```java
import java.time.LocalDateTime;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDateTime currentDateTime = LocalDateTime.now();
        System.out.println(currentDateTime); // 例出力: 2023-04-01T12:34:56.789
    }
}
```
### `java.util.Date` を使用する（レガシー）
```java
import java.util.Date;

public class CurrentDateExample {
    public static void main(String[] args) {
        Date currentDate = new Date();
        System.out.println(currentDate); // 例出力: Sat Apr 01 12:34:56 BST 2023
    }
}
```
### サードパーティライブラリの利用: Joda-Time
Java 8以前では、Joda-TimeはJavaでの日付と時間に関する事実上の標準でした。レガシーシステムで作業している場合やJoda-Timeを好む場合は、現在の日付を取得するためにこう使うことができます：
```java
import org.joda.time.LocalDate;

public class CurrentDateExample {
    public static void main(String[] args) {
        LocalDate currentDate = LocalDate.now();
        System.out.println(currentDate); // 例出力: 2023-04-01
    }
}
```
**注記：** `java.util.Date` とJoda-Timeはまだ使用されていますが、`java.time` パッケージはその不変性と日付と時刻を扱うための包括的なAPIのため、新しいプロジェクトには推奨されます。
