---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

コンピュータ上で現在の日付を取得することは、多くのプログラムやアプリケーションで一般的な操作です。これは、ログのタイムスタンプ、データの時間制限、スケジューリングなど、様々な目的で使用されます。

## 方法:

Javaで現在の日付を取得する基本的な方法を以下に示します。

```Java
import java.time.LocalDate;

public class Main {
  public static void main(String[] args) {
    LocalDate date = LocalDate.now();
    System.out.println("Today's date is: " + date);
  }
}
```
実行すると、以下のような出力が得られます：
```
Today's date is: 2021-12-13
```

## ディープダイブ

Javaでは、`java.util.Date`が元々日付および時刻を扱うためのクラスとして提供されていましたが、その設計は多くの問題を抱えていました。そのため、Java 8で導入された`java.time`パッケージが現在では広く推奨されています。

日付を入手する別の方法として`java.util.Calendar`があります。しかし、これも設計問題があるため、可能ならば新しいAPIを使用することが推奨されます。

`LocalDate.now()`メソッドの実装は、基本的にシステムクロックを使用して現在の日付を取得します。

## 関連リンク

以下のリンクで、Javaの日付と時刻に関するさらに詳しい情報を得ることができます：

1. [Oracle Official Documentation](https://docs.oracle.com/javase/tutorial/datetime/)