---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:37:03.867342-07:00
simple_title:         "文字列から日付を解析する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## 何となぜ？

StringからDateへのパースは、テキスト形式の日付をプログラムが理解できるDate型に変換することです。データを処理、表示、または保存する際に役立つため、プログラマーがよく行います。

## How to:
## 方法：

```java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateParser {
    public static void main(String[] args) {
        try {
            SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
            String dateString = "2023-04-01";
            Date date = formatter.parse(dateString);
            System.out.println("Parsed Date: " + date);
        } catch (Exception e) {
            System.out.println("Error parsing the date.");
        }
    }
}
```

Sample Output:
```
Parsed Date: Sat Apr 01 00:00:00 JST 2023
```

## Deep Dive
## 詳細情報：

DateパーサはJavaに長く存在します。かつては`java.util.Date`と`SimpleDateFormat`が一般的でしたが、不完全で、スレッドセーフでないなどの問題がありました。Java 8以降、`java.time`パッケージが導入され、`LocalDate`, `LocalDateTime`, `DateTimeFormatter`などの安全で使いやすい代替案が提供されています。

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class NewDateParser {
    public static void main(String[] args) {
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        String dateString = "2023-04-01";
        LocalDate date = LocalDate.parse(dateString, formatter);
        System.out.println("Parsed Date: " + date);
    }
}
```

Sample Output:
```
Parsed Date: 2023-04-01
```

旧APIと比較して、`java.time`はイミュータブルで、日付と時間をより正確に表現できます。必要に応じて、異なるタイムゾーンやロケールでの操作も容易です。

## See Also
## 参考情報：

- [Date and Time Classes in Java](https://docs.oracle.com/javase/tutorial/datetime/)
- [`SimpleDateFormat` Documentation](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [`DateTimeFormatter` Documentation](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Java 8 Date/Time Guide](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
