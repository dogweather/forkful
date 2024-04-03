---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:26.323381-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.960633-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u306E\u30C6\u30AD\u30B9\u30C8\u8868\
  \u73FE\u3092`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u307E\u305F\u306F\u3088\u308A\
  \u73FE\u4EE3\u7684\u306A`LocalDateTime`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\
  \u5909\u63DB\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3001\u65E5\u4ED8\u306E\u8A08\u7B97\u3001\u691C\u8A3C\u3001\u307E\u305F\
  \u306F\u4E00\u8CAB\u3057\u305F\u56FD\u969B\u5316\u3092\u8981\u6C42\u3059\u308B\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u91CD\u8981\u306A\
  \u3001\u6A19\u6E96\u5316\u3055\u308C\u305F\u5F62\u5F0F\u3067\u65E5\u4ED8\u3092\u64CD\
  \u4F5C\u3001\u5F62\u5F0F\u8A2D\u5B9A\u3001\u6BD4\u8F03\u3001\u307E\u305F\u306F\u4FDD\
  \u5B58\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\
  ."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 何となぜ？
文字列から日付を解析するとは、日付と時刻のテキスト表現を`Date`オブジェクトまたはより現代的な`LocalDateTime`オブジェクトに変換することです。プログラマーは、日付の計算、検証、または一貫した国際化を要求するアプリケーションにとって重要な、標準化された形式で日付を操作、形式設定、比較、または保存するためにこれを行います。

## 方法：

### `java.time`パッケージを使用する（Java 8以降で推奨）:
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // 出力: 2023-04-30
    }
}
```

### `SimpleDateFormat`を使用する（古いアプローチ）:
```java
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "30/04/2023";
        SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
        try {
            Date date = formatter.parse(dateString);
            System.out.println(date); // 出力形式はシステムのデフォルト形式に依存します
        } catch (ParseException e) {
            e.printStackTrace();
        }
    }
}
```

### サードパーティライブラリを使用する（例：Joda-Time）:
Joda-Timeは重要なサードパーティライブラリでしたが、Java 8の`java.time`パッケージの導入により、現在はメンテナンスモードです。しかし、Java 8より前のバージョンを使用している場合、Joda-Timeは良い選択肢です。
```java
import org.joda.time.LocalDate;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;

public class DateParser {
    public static void main(String[] args) {
        String dateString = "2023-04-30";
        DateTimeFormatter formatter = DateTimeFormat.forPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println(date); // 出力: 2023-04-30
    }
}
```
日付を扱う際は、日付だけでなく日時を解析または形式設定する場合は常にタイムゾーンの設定に注意してください。
