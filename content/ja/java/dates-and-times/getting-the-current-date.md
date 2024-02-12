---
title:                "現在の日付の取得"
date:                  2024-02-03T19:10:04.111937-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
