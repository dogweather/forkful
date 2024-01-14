---
title:                "Java: 日付を文字列に変換する"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由について説明します。日付のデータを文字列に変換することで、様々なフォーマットや形式で表示することができます。例えば、必要な情報を特定の形式で表現する場合や、データベースに保存する際に文字列として扱いやすくするためなど、日付を文字列に変換する必要があります。

## 方法
日付を文字列に変換するためのコーディング例と出力結果を```Java ... ```のコードブロックを使用して示します。

```Java
// 日付を表すDateオブジェクトの生成
Date date = new Date();

// SimpleDateFormatクラスを使用して日付を任意の書式で表示
SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd");

// 日付を文字列に変換
String dateString = sdf.format(date);

// 変換した文字列を出力
System.out.println("変換後の日付：" + dateString);
```

出力結果：
```
変換後の日付：2020/09/18
```

## ディープダイブ
日付を文字列に変換する際には、様々なフォーマットやオプションを指定することができます。例えば、時間や曜日を含めるかどうか、タイムゾーンを考慮するかどうかなど、細かい設定を行うことができます。また、文字列から日付を逆変換する方法についても学ぶことができます。

## See Also
- [Javaで文字列を日付に変換する方法](https://techacademy.jp/magazine/17940)
- [Javaの日付・時刻を扱うAPIの基本](https://www.javadrive.jp/start/date/index1.html)
- [SimpleDateFormat Class in Java](https://www.geeksforgeeks.org/simpledateformat-class-in-java-with-examples/)