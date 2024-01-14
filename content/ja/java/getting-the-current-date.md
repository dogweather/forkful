---
title:    "Java: 現在の日付の取得"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することに興味を持つ理由はさまざまです。例えば、プロジェクトの期限を管理するために日付を取得したり、特定の日付に関連するデータを処理する必要がある場合に、取得する必要があります。

## 方法

Javaでは、現在の日付を取得する方法がいくつかあります。最も一般的な方法は、`LocalDate.now()`を使用することです。これは、コンピューターのシステム時計を使用して現在の日付を取得します。以下のコードブロックに、`LocalDate.now()`を使用した例を示します。

```Java
import java.time.LocalDate;

public class GetCurrentDate {
  public static void main(String[] args) {
    // LocalDate.now()を使用して、現在の日付を取得する
    LocalDate currentDate = LocalDate.now();
    
    // 取得した日付を出力する
    System.out.println("現在の日付は" + currentDate + "です");
  }
}
```

このコードを実行すると、以下のような出力が得られます。

```
現在の日付は2021-09-16です
```

また、特定のタイムゾーンに基づいて現在の日付を取得したい場合は、`ZonedDateTime.now(zoneId)`を使用します。以下のコードブロックに、アメリカ東部標準時に基づいて現在の日付を取得する例を示します。

```Java
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class GetCurrentDate {
  public static void main(String[] args) {
    // アメリカ東部標準時のタイムゾーンを設定する
    ZoneId zoneId = ZoneId.of("America/New_York");
    
    // ZonedDateTime.now(zoneId)を使用して、現在の日付を取得する
    ZonedDateTime currentDate = ZonedDateTime.now(zoneId);
    
    // 取得した日付を出力する
    System.out.println("現在の日付は" + currentDate + "です");
  }
}
```

このコードを実行すると、以下のような出力が得られます。

```
現在の日付は2021-09-16T06:00-04:00[America/New_York]です
```

## 深堀り

現在の日付を取得する方法にはさまざまなオプションがあります。例えば、`LocalDate.now()`や`ZonedDateTime.now(zoneId)`以外にも、`LocalDateTime.now()`や`OffsetDateTime.now(offset)`などがあります。また、取得した日付をより詳細に扱うためには、`LocalDate`や`ZonedDateTime`などのメソッドを使用することもできます。

詳細な情報やより高度な操作については、Javaの公式ドキュメントを参照することをお勧めします。また、日付と時刻を一緒に扱う場合には、`java.time`パッケージを使用することが推奨されています。

## 参考リンク

- [Javaの公式ドキュメント (日付と時刻)](https://docs.oracle.com/javase/jp/8/docs/api/java/time/package-summary.html)
- [Javaで日付や時間を処理する方法](https://www.javadrive.jp/start/date/index1.html)
- [初めてのJava8日付と時刻API](https://www.baeldung.com/java-8-date-time-intro)