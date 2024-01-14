---
title:                "Java: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することに興味はありますか？文字列に変換することで、日付をプログラムで扱いやすくすることができます。例えば、日付をデータベースに保存する際や、日付を検索する際に、文字列として扱った方が便利な場合があります。

## 方法
では、日付を文字列に変換する方法を見ていきましょう。まずは下のコードブロックを見てください。

```Java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {

    public static void main(String[] args) {

        // 日付を作成
        LocalDate date = LocalDate.now();

        // フォーマットを指定
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy年MM月dd日");

        // 日付を文字列に変換
        String dateStr = date.format(formatter);

        // 出力
        System.out.println(dateStr); // 現在の日付が「2020年08月27日」の場合、出力は「2020年08月27日」となる
    }
}
```

上の例では、`LocalDate`クラスを使用して日付を作成し、`DateTimeFormatter`クラスを使用してフォーマットを指定し、`format()`メソッドで日付を文字列に変換しています。このように、Javaでは日付を操作するための多くのクラスやメソッドが用意されているので、日付を文字列に変換することは意外と簡単です。

## ディープダイブ
さらに、日付を文字列に変換する際によく使われる`DateTimeFormatter`クラスの`ofPattern()`メソッドについて深く掘り下げてみましょう。このメソッドには、パターン文字列を使用して日付のフォーマットを指定することができます。例えば、`yyyy`は年、`MM`は月、`dd`は日を表すパターン文字列です。

また、フォーマットにはロケール（地域や言語）を指定することもできます。例えば、日本語のロケールを指定すると、「8月」が「08月」と表記されるようになります。

日付を文字列に変換する際に、フォーマットを指定することで自分の好みに合った形式で日付を表示することができます。

## 参考リンク
- [Java 8の日付と時刻 API 基礎](https://java.keicode.com/lang/date-time-api-basic.php)
- [Java 8でよく使う日付・時刻処理のやり方（かたち）](https://qiita.com/yukin01/items/1a3439c1104827d1814f)
- [DateTimeFormatter (Java Platform SE 8)](https://docs.oracle.com/javase/jp/8/docs/api/java/time/format/DateTimeFormatter.html)

## さらに見る
- [Markdownとは？](https://ja.wikipedia.org/wiki/Markdown)
- [日付を文字列に変換する他の方法](https://www.it-swarm-ja.tech/ja/java/%E6%97%A5%E4%BB%98%E3%82%92%E6%96%87%E5%AD%97%E5%88%97%E3%81%AB%E5%A4%89%E6%8F%9B%E3%81%99%E3%82%8B/961714000/)