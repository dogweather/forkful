---
title:                "Java: 現在の日付の取得"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

# なぜ現在の日付を取得するのか

現代の世界では、私たちは日々さまざまなデジタルサービスを利用しています。その中で、日付や時刻を示すことは非常に重要です。例えば、メッセージのタイムスタンプや予定の管理など、日付や時刻は私たちの生活の中で欠かせないものです。そのため、Javaプログラミングで日付の取得方法を学ぶことはとても価値があります。

## 使い方

日付を取得するためには、Javaの `Date` クラスや `Calendar` クラスを使うことができます。例えば、以下のようなコードを書くことで、現在の日付と時刻を表示することができます。

```Java
// Dateクラスを使った例
Date date = new Date();
System.out.println(date);

// Calendarクラスを使った例
Calendar calendar = Calendar.getInstance();
System.out.println(calendar.getTime());
```

上記のコードを実行すると、次のような出力が表示されます。

```
Sun Feb 28 18:07:40 JST 2021
Sun Feb 28 18:07:40 JST 2021
```

また、より細かい日付や時刻の情報を取得するためには、`DateFormat` クラスや `SimpleDateFormat` クラスを使用することができます。例えば、以下のようにフォーマットを指定することで、特定の形式で日付や時刻を表示することができます。

```Java
SimpleDateFormat sdf = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");
System.out.println(sdf.format(date));
System.out.println(sdf.format(calendar.getTime()));
```

上記のコードを実行すると、次のような出力が表示されます。

```
2021/02/28 18:07:40
2021/02/28 18:07:40
```

## 深堀り

Javaで日付や時刻を取得する方法はさまざまありますが、内部的には`System.currentTimeMillis()` メソッドが呼び出され、エポックタイム（1970年1月1日午前0時からのミリ秒単位の経過時間）を取得しています。そのため、実際には現在の日付や時刻を取得するために、システムの時間をベースとして計算を行っていることになります。

また、Java 8からは新しい日付と時刻のAPIが導入され、`LocalDate` や `LocalTime`、`LocalDateTime` クラスが使えるようになりました。これらのクラスを使用することで、より柔軟かつ簡単に日付や時刻を扱うことができます。

## 関連リンク

- [Javaの公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/util/Date.html)
- [Javaの日付と時間の処理方法](https://www.javadrive.jp/start/date/index1.html)
- [Java8で導入された日付と時間の新API](https://www.atmarkit.co.jp/ait/articles/1502/20/news032.html)

# 関連情報の参照

- [Markdown 記法の日本語インフォグラフィックス](https://qiita.com/oreo/items/82183bfbaac69971917f)
- [Markdown記法 チートシート](https://qiita.com/Qiita/items/c686397e4a0f4f11683d)