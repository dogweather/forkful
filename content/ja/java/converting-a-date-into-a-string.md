---
title:                "日付を文字列に変換する"
html_title:           "Java: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
日付を文字列に変換することの最大の利点は、日付をより視覚的に理解しやすくなることです。また、文字列として扱うことで、様々なデータの処理や表示がよりスムーズに行えるようになります。

## How To
日付を文字列に変換するためには、Javaの`SimpleDateFormat`クラスを使用します。以下のコードブロックは、日付を指定した形式の文字列に変換する例です。

```Java
import java.text.SimpleDateFormat;
import java.util.Date;

public class DateFormatting {
    public static void main(String[] args) {
        Date today = new Date();  // 現在の日付を取得
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd");  // 日付のフォーマットを定義
        String dateString = formatter.format(today);  // 日付を文字列に変換
        System.out.println("Today's date is: " + dateString);  // 日付を出力
    }
}
```
上記のコードを実行すると、以下のような出力が得られます。

```
Today's date is: 2021/07/15
```

日付を文字列に変換する際には、`SimpleDateFormat`クラスのコンストラクタに渡す引数で、変換したい日付のフォーマットを指定します。例えば、月や曜日も表示したい場合は`"yyyy年MM月dd日（E）"`というように指定することができます。さまざまなフォーマットを試してみて、自分に合ったものを選択しましょう。

## Deep Dive
日付を文字列に変換する際、文字列から日付に戻すことも可能です。その際には、`SimpleDateFormat`クラスの`parse()`メソッドを使用します。以下のようなコードを追加することで、文字列から日付に変換することができます。

```Java
String input = "2021/07/15";
SimpleDateFormat formatter = new SimpleDateFormat("yyyy/MM/dd");
Date date = formatter.parse(input);  // 文字列を日付に変換
System.out.println("Converted date is: " + date);  // 日付を出力
```

また、`SimpleDateFormat`クラスではパターン文字と呼ばれる特殊文字を使用して、フォーマットを自由に設定することができます。例えば、`yyyy`は年を表し、小文字の`m`は月を表します。パターン文字を組み合わせることで、さまざまな日付のフォーマットが可能になります。

## See Also
この記事では、Javaで日付を文字列に変換する方法について紹介しました。他にも、Javaに関する役立つ情報を以下のリンクから参照することができます。

- [Javaで日付と時刻を扱う方法について](https://www.tuyano.com/index3?id=2906003)
- [Javaのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/)
- [Javaのチュートリアル](https://docs.oracle.com/javase/tutorial/)