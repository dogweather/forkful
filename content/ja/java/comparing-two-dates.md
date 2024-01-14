---
title:    "Java: 2つの日付を比較する"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

あなたが日常のプログラミング作業中に日付を比較したい理由はたくさんあります。例えば、特定の期間内に実行された処理を特定するためや、使用期限が切れたアカウントを自動的に無効にするためなど、様々なケースがあります。そこで、今回はJavaで日付を比較する方法をご紹介します。

## 方法

日付を比較するには、まずは比較する二つの日付をそれぞれJavaのDateクラスのインスタンスとして取得します。例えば、今日の日付を取得するコードは以下のようになります。

```Java
	Date today = new Date();
```

次に、比較したい日付を適切なフォーマットに変換します。例えば、文字列から日付を生成するにはSimpleDateFormatクラスを使用します。

```Java
	SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
	
	Date date = formatter.parse("2021-10-01");
```

最後に、compareToメソッドを使用して二つの日付を比較します。このメソッドは、比較したい日付を引数として受け取り、比較結果を整数で返します。返される値によって、比較結果がわかります。

```Java
	int result = date.compareTo(today);
	
	if(result > 0) {
		System.out.println("date is after today");
	} else if(result < 0) {
		System.out.println("date is before today");
	} else {
		System.out.println("date is same as today");
	}
```

このように、SimpleDateFormatやcompareToメソッドを利用することで、日付を比較することができます。

## 深堀り

比較に使用するDateクラスは、java.utilパッケージに含まれています。しかし、このクラスは古くから使われており、利用できるメソッドが限られていたり、パフォーマンス面での課題があります。そのため、Java 8からは新しい日付APIであるLocalDateクラスが導入されました。

LocalDateクラスは、ISO-8601の日付形式をサポートしており、特定の期間内の日付のチェックや、特定の曜日の日付を取得するなど、日付操作に便利です。また、内部では不変オブジェクトを使用するため、スレッドセーフです。

比較についても、LocalDateクラスはcompareToメソッドのほかに、isAfterやisBeforeなどのメソッドも提供しています。さらに、TemporalAdjustersクラスを使用して、特定の日付を簡単に取得することもできます。

## この記事を参考にする

今回はJavaで日付を比較する方法についてご紹介しました。日付操作はプログラミングに欠かせない部分ですので、ぜひこの記事を参考にしてみてください。

## 関連リンク

- [Java Dateクラスのドキュメンテーション](https://docs.oracle.com/javase/jp/8/docs/api/java/util/Date.html)
- [Java LocalDateクラスのドキュメンテーション](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDate.html)
- [Java 8で新しく導入された日付APIの解説記事](https://www.ibm.com/developerworks/jp/java/library/j-javadevapi8-1/)