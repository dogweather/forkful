---
title:                "日付の文字列への変換"
html_title:           "Java: 日付の文字列への変換"
simple_title:         "日付の文字列への変換"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
日付を文字列に変換することは、プログラマーが一般的に行うことです。これは、日付をより読みやすくするために行われます。例えば、2021年10月1日は10/1/2021や2021年10月01日のように表記されることがありますが、日付を文字列に変換することで、より統一的な形式で表示させることができます。

## 方法：
日付を文字列に変換する方法については、Javaの標準ライブラリであるjava.timeパッケージを使用することができます。以下の例では、LocalDateクラスを使用して、日付を文字列に変換し、その出力結果を表示しています。

```Java
import java.time.LocalDate;

public class DateToStringExample {

    public static void main(String[] args) {
        // 日付を設定
        LocalDate date = LocalDate.of(2021, 10, 1);

        // 日付を文字列に変換
        String dateString = date.toString();

        // 出力
        System.out.println("日付：" + dateString);
    }
}
```
出力結果： 日付：2021-10-01

## 詳細を掘り下げる：
日付を文字列に変換する方法には、java.timeパッケージ以外にも、SimpleDateFormatやDateTimeFormatterクラスなどを使用する方法もあります。また、日付を文字列に変換する際には、表示したいフォーマットや言語に応じて、適切なパターンを選択する必要があります。

## 関連情報：
- [java.timeパッケージの公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [SimpleDateFormatクラスの公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [DateTimeFormatterクラスの公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)