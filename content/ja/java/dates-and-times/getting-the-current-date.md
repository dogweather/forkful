---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:04.111937-07:00
description: "\u65B9\u6CD5\uFF1A Java\u3067\u306F\u3001\u53E4\u3044 `java.util.Date`\
  \ \u30AF\u30E9\u30B9\u3068\u3001\u3088\u308A\u591A\u69D8\u3067\u76F4\u611F\u7684\
  \u306A\u65B0\u3057\u3044 `java.time` \u30D1\u30C3\u30B1\u30FC\u30B8\uFF08Java 8\u3067\
  \u5C0E\u5165\uFF09\u306E\u4E21\u65B9\u3092\u4F7F\u7528\u3057\u3066\u3001\u73FE\u5728\
  \u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u8907\u6570\u306E\u65B9\u6CD5\u304C\
  \u3042\u308A\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.854923-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
