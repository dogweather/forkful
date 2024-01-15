---
title:                "現在の日付の取得"
html_title:           "Java: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

Javaの最新バージョンを使って現在の日付を取得することに興味があるかもしれません。その日付を利用して、プログラムの実行日時を記録したり、特定のタスクが実行された日付を記録したりすることができます。

## 方法

Javaでは、`LocalDate`クラスを使用して簡単に現在の日付を取得することができます。以下のコードを使用して、現在の日付を取得し、コンソールに表示することができます。

```Java
LocalDate currentDate = LocalDate.now();
System.out.println("今日の日付は：" + currentDate);
```

出力結果は、現在の日付が年-月-日の形式で表示されます。例えば、今天の日付が2021年9月12日ならば、出力結果は `今日の日付は：2021-09-12` となります。

また、`LocalDate`クラスには、日付を指定したい場合に使用できる様々なメソッドがあります。例えば、`of`メソッドを使用すると、指定した年月日の日付を取得することができます。

```Java
LocalDate specificDate = LocalDate.of(2021, 12, 25);
System.out.println("指定した日付は：" + specificDate);
```

出力結果は、 `指定した日付は：2021-12-25` となります。

## ディープダイブ

`LocalDate.now()`メソッドでは、現在の日付を取得するだけでなく、時間やタイムゾーンの情報も取得することができます。また、`LocalDate`クラスの他にも、日付や時間を表すクラスとして、`LocalDateTime`や`ZonedDateTime`などもあります。

さらに、Java 8からは、`java.time`パッケージが導入され、より便利な日付処理を行うことができるようになりました。`LocalDate`クラスの他にも、`Period`や`ChronoUnit`などのクラスやメソッドを使用することで、より柔軟な日付計算を行うことができます。

## 関連情報

- [Java 11 公式ドキュメント - 日時 API](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/package-summary.html)
- [プログラミングの基礎知識 - 日付処理](https://www.javadrive.jp/start/date/index1.html)
- [技術評論社 - Java入門 入門編 第3章 日付やファイル操作について学ぶ](https://gihyo.jp/dev/serial/01/java_basic)
- [JDK公式サイト - 新しい日時APIの概要](https://openjdk.java.net/projects/jdk8/javadoc/java.time-summary.html#NewAPIsOverview)