---
title:                "Java: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
日付を計算することの利点は、将来または過去の特定の日付をすばやく特定できることです。

## 方法
日付を計算する方法は次のようになります。まず、Calendarクラスを使用してインスタンスを作成し、そのインスタンスに日付の情報をセットします。その後、そのインスタンスを使用して、簡単な計算を行うことによって、特定の日付を取得することができます。以下のコードを参考にしてください。

```Java
// Calendarクラスを使用してインスタンスを作成
Calendar calendar = Calendar.getInstance();
// 日付の情報をセット
int year = 2021;
int month = Calendar.JULY; //7月を表す
int day = 24;
calendar.set(year, month, day);

// 特定の日付を取得
int futureDate = 30;
calendar.add(Calendar.DATE, futureDate); // 30日後の日付を取得
int pastDate = -15;
calendar.add(Calendar.DATE, pastDate); // 15日前の日付を取得

// 結果を出力
System.out.println("特定の日付の30日後：" + calendar.getTime());
System.out.println("特定の日付の15日前：" + calendar.getTime());
```

以下のような出力が得られます。

```shell
特定の日付の30日後：Tue Aug 24 00:00:00 JST 2021 
特定の日付の15日前：Wed Jul 07 00:00:00 JST 2021
```

## ディープダイブ 
日付を計算するために使用される主なクラスは、Calendarクラスです。このクラスは抽象クラスであり、システムが動作しているローカルタイムゾーンに基づいたカレンダーを表します。そのため、使用するコンピューターのタイムゾーンに応じて結果が異なる可能性があります。また、Calendarクラスには、DATEやMONTHなど、日付や月を表す定数がありますので、使用する際には参照すると便利です。

## See Also
- [Java公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/util/Calendar.html)
- [Calendarクラスを使用した日付計算のサンプルコード](https://www.java67.com/2015/06/how-to-calculate-add-subtract-days-in-date-java-example.html)
- [Javaの日付を操作する方法](https://www.geeksforgeeks.org/how-to-add-days-to-a-date-in-java/)