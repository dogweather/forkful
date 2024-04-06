---
date: 2024-01-20 17:37:48.752006-07:00
description: "How to: (\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u3092\u5B9F\
  \u884C\u3059\u308B\u3068\u3001\u30B3\u30F3\u30BD\u30FC\u30EB\u306B\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3068\u6642\u523B\u304C `yyyy/MM/dd HH:mm:ss` \u306E\u5F62\u5F0F\u3067\
  \u8868\u793A\u3055\u308C\u308B\u3088\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.855988-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u3092\u5B9F\u884C\u3059\
  \u308B\u3068\u3001\u30B3\u30F3\u30BD\u30FC\u30EB\u306B\u73FE\u5728\u306E\u65E5\u4ED8\
  \u3068\u6642\u523B\u304C `yyyy/MM/dd HH:mm:ss` \u306E\u5F62\u5F0F\u3067\u8868\u793A\
  \u3055\u308C\u308B\u3088\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (やり方)
```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatExample {
    public static void main(String[] args) {
        Date date = new Date();
        
        // SimpleDateFormat を使って日付を文字列に変換
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
        String strDate = formatter.format(date);
        System.out.println(strDate); // 例：2023/03/28 15:45:30
    }
}
```

このコードを実行すると、コンソールに現在の日付と時刻が `yyyy/MM/dd HH:mm:ss` の形式で表示されるよ。

## Deep Dive (深掘り)
日付と文字列の変換はJavaが初期に登場してから必要とされていた。`SimpleDateFormat`はその一例で、Java 1.1から利用可能だ。ただ、`Thread`セーフじゃないから、Java 8以降では`DateTimeFormatter`を使うことが推奨されている。

```java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class ModernDateFormatExample {
    public static void main(String[] args) {
        LocalDateTime dateTime = LocalDateTime.now();
        
        // DateTimeFormatter を使った新しいやり方
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
        String formattedDateTime = dateTime.format(formatter);
        System.out.println(formattedDateTime); // 例：2023/03/28 15:50:10
    }
}
```

`SimpleDateFormat`と比べて、`DateTimeFormatter`は不変であり、かつスレッドセーフ。パフォーマンスや安全性を理由に、現代のJavaコードでは`DateTimeFormatter`の使用が望ましい。

## See Also (関連情報)
- [SimpleDateFormat documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/text/SimpleDateFormat.html)
- [DateTimeFormatter documentation](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/time/format/DateTimeFormatter.html)
- [Java Date and Time - Baeldung](https://www.baeldung.com/java-8-date-time-intro)

さらなる情報が必要なら、上のリンクをチェックしてみて。日付と時間に関するJavaのAPIをより深く学べるよ。
