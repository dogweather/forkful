---
aliases:
- /ja/java/converting-a-date-into-a-string/
date: 2024-01-20 17:37:48.752006-07:00
description: "Java\u3067\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\
  \u308B\u3063\u3066\u3001\u5177\u4F53\u7684\u306B\u306F`Date`\u30AA\u30D6\u30B8\u30A7\
  \u30AF\u30C8\u3092\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u3059\u308B\u3053\u3068\
  \u3060\u3088\u3002\u306A\u305C\u3053\u308C\u304C\u5FC5\u8981\u304B\u3063\u3066\uFF1F\
  \u30C7\u30FC\u30BF\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u308F\u304B\u308A\u3084\u3059\
  \u304F\u8868\u793A\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u5F62\u5F0F\u3067\u4FDD\
  \u5B58\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u3055\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.812097
model: gpt-4-1106-preview
summary: "Java\u3067\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\
  \u3063\u3066\u3001\u5177\u4F53\u7684\u306B\u306F`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u3092\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u3059\u308B\u3053\u3068\u3060\
  \u3088\u3002\u306A\u305C\u3053\u308C\u304C\u5FC5\u8981\u304B\u3063\u3066\uFF1F\u30C7\
  \u30FC\u30BF\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u308F\u304B\u308A\u3084\u3059\u304F\
  \u8868\u793A\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u5F62\u5F0F\u3067\u4FDD\u5B58\
  \u3057\u305F\u308A\u3059\u308B\u305F\u3081\u3055\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (なにを、そしてなぜ？)

Javaで日付を文字列に変換するって、具体的には`Date`オブジェクトをテキスト形式にすることだよ。なぜこれが必要かって？データをユーザーにわかりやすく表示したり、特定の形式で保存したりするためさ。

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
