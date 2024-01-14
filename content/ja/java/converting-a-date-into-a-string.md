---
title:    "Java: 日付を文字列に変換する"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# なぜ日付を文字列に変換するのか

日付を文字列に変換する必要がある理由はさまざまです。例えば、データベースのクエリを実行する際に日付を文字列に変換することで、より柔軟な検索が可能になります。また、日付を文字列として表示することで、ユーザーが理解しやすくなります。

## 方法

日付を文字列に変換するには、JavaのDateクラスやLocalDateTimeクラスを使用します。下記に例を示します。

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateToStringExample {

    public static void main(String[] args) {
        // 日付を取得
        Date date = new Date();
        System.out.println("日付: " + date);

        // フォーマットを指定して文字列に変換
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");
        String dateString = sdf.format(date);
        System.out.println("文字列: " + dateString);
    }
}
```
上記のコードを実行すると、以下のような出力が得られます。

```
日付: Fri Jan 22 12:00:00 JST 2021
文字列: 2021/01/22
```

さらに、日付を指定した形式で文字列に変換する方法もあります。例えば、月や曜日を表す英語の表記を日本語の表記に変換することができます。下記にコード例を示します。

```Java
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Locale;

public class DateToStringExample {

    public static void main(String[] args) {
        // 日付を取得
        LocalDateTime dateTime = LocalDateTime.now();
        System.out.println("日付: " + dateTime);

        // パターンを指定して文字列に変換
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd (E) a h:mm", Locale.JAPANESE);
        String dateString = dateTime.format(formatter);
        System.out.println("文字列: " + dateString);
    }
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
日付: 2021-01-22T12:00:00
文字列: 2021/01/22 (金) 午後 12:00
```

## 詳細情報

日付を文字列に変換する方法はさまざまありますが、基本的な考え方は同じです。日付を取得し、指定したフォーマットに従って文字列に変換するのです。また、Java 8以降では、日付を取得するにはLocalDateTimeクラスを使用することが推奨されています。

# 参考リンク

- [Java: 日付を文字列に変換する方法](https://www.sejuku.net/blog/57510)
- [Java8で現在日時を扱う – LocalDateTimeクラスの使い方](https://teratail.com/questions/73246)
- [DateTimeFormatter Class in Java 8](https://www.javatpoint.com/java-8-datetimeformatter)