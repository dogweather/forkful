---
title:                "文字列から日付を解析する"
html_title:           "Bash: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)

文字列から日付を解析するとは、文字列を日付データ型に変換する行為を指します。これにより、プログラマは日付関連の操作（例：加算、減算、間隔計算）を簡単に行うことができます。

## やり方 (How to)

まず、java.timeパッケージのLocalDateクラスとDateTimeFormatterクラスを使用する例を見てみましょう。

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class Main {
    public static void main(String[] args) {
        String strDate = "2022-10-30";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");
        LocalDate date = LocalDate.parse(strDate, formatter);

        System.out.println(date);
    }
}
```

出力結果は以下の通りです。

```
2022-10-30
```

## 深掘り (Deep Dive)

歴史的には、java.utilパッケージのSimpleDateFormatクラスが文字列から日付を解析するためによく使われていましたが、Java 8以降では新しいjava.timeパッケージが推奨されています。その理由は以下の通りです。

1. スレッドセーフ：SimpleDateFormatはスレッドセーフではないため、マルチスレッド環境では問題を引き起こす可能性があります。一方、java.timeパッケージはスレッドセーフです。

2. ローカル化：java.timeAPIは明示的なローカリゼーションをサポートしています。

3. モデル化：Java 8の日付/時間APIはISOカレンダーシステムに基づくモデルを導入しました。

代替として、Joda-Timeライブラリも存在しますが、Java 8以降ではjava.timeの使用が推奨されています。

## 参照 (See Also)

- Oracle公式マニュアル：日付と時間 ([こちら](https://docs.oracle.com/javase/tutorial/datetime/index.html))
- Joda-Timeライブラリ ([こちら](https://www.joda.org/joda-time/))