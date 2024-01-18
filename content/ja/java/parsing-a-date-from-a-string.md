---
title:                "文字列から日付を解析する"
html_title:           "Java: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# なに＆なぜ？
日本人の読者の皆さん、こんにちは！今回はJavaで日付を文字列からパースする方法についてお話します。パースとは、日付を文字列から特定の形式に変換することを意味します。プログラマー達がこれをする理由は、日付をより扱いやすい形にするためです。例えば、データベースに格納されている日付を特定の形式で表示したり、入力された日付を特定の形式で処理したりする必要がある場合に使われます。

## ハウツー
まずは、```LocalDate```クラスを使用して日付を文字列からパースする方法を紹介します。

```Java
String dateStr = "2021-01-01";
LocalDate date = LocalDate.parse(dateStr); // dateStrをLocalDate型に変換
System.out.println(date); // 2021-01-01
```

次に、日付に特定のフォーマットを指定してパースする方法を見ていきましょう。

```Java
String dateStr = "2021/01/01";
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd"); // フォーマットを指定
LocalDate date = LocalDate.parse(dateStr, formatter); // dateStrをフォーマットに合わせてLocalDate型に変換
System.out.println(date); // 2021-01-01
```

## ディープダイブ
日付を文字列からパースする方法はJava 8で導入されました。それ以前のバージョンでは、特定のフォーマットに合わせて日付をパースするために、```SimpleDateFormat```クラスが使用されていました。

他の言語でも、日付を文字列からパースするための類似のメソッドがあります。例えば、JavaScriptでは```Date.parse()```メソッドがありますが、こちらはISOフォーマット以外の日付は正しくパースすることができません。

日付を文字列からパースする際には、入力された文字列のフォーマットに合わせてパースする必要があります。フォーマットが間違っていると、正しい日付としてパースすることができません。

## 関連リンク
- [Oracle documents: LocalDate class](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDate.html)
- [Java SE 8 の新機能: 日付と時間 API](https://www.oracle.com/jp/technical-resources/articles/java/se-jp/java-se-8-new-features.html#dateandtimeapi)
- [Java Date to LocalDate](https://www.baeldung.com/java-date-to-localdate)