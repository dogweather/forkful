---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:26.323381-07:00
description: ''
lastmod: '2024-04-05T21:59:54.288376-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
