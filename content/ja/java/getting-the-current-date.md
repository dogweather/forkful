---
title:                "現在の日付を取得する"
html_title:           "Java: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 今日の日付を取得する理由
プログラマーにとって、現在の日付を取得することは非常に重要です。それは、コンピューターやネットワーク上のイベントの時間を記録するために使用されるからです。また、プログラム内で特定の日付を確認することで、特定の処理を行う必要がある場合に役立ちます。

## やり方：
```
Java LocalDateTime datetime = LocalDateTime.now(); 
System.out.println(datetime);
```
Output: 2021-08-04T15:30:12.875

取得した日付をフォーマットすることもできます。
```
SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd"); 
Date date = new Date(); 
System.out.println(sdf.format(date));
```
Output: 2021/08/04

## 詳しく見る
(1) 歴史的な背景：Java 1.8以前では、DateクラスやCalendarクラスを使用して日付を取得していましたが、現在ではLocalDateTimeクラスを使用することが推奨されています。
(2) 代替手段：Java以外にも、PythonやC++などの他のプログラミング言語でも日付を取得する方法は存在します。
(3) 実装の詳細：JVM（Java Virtual Machine）内のシステム時計にアクセスして現在の日付を取得します。また、現在のローカル時刻やタイムゾーンも取得することができます。

## 関連情報を見る
- [Java 8 LocalDateTime Class](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html)
- [Java SimpleDateFormat Class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)