---
title:    "Clojure: 「日付の比較」"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ
Clojureプログラミングの世界では、日付の比較はよく使用されるタスクです。例えば、アプリケーションで過去の日付と現在の日付を比較するといった場面があります。そのような場面で、日付の比較を正しく行うことが重要です。この記事では、二つの日付を比較する際に必要なことを紹介します。

## 方法
日付の比較は、Clojureで組み込みの```<```や```>```といった演算子を使うことで簡単に行うことができます。例えば、以下のコードを見てください。

```Clojure
(def date1 (local-date 2019 10 01))
(def date2 (local-date 2019 09 01))

(date1 > date2) ;=> true
```

このように、演算子を使うことで二つの日付を比較することができます。

## 深く掘り下げる
Clojureでは、日付を表すデータ型として```java.time.LocalDate```が使われます。この型を使うことで、日付の比較だけでなく、日付の計算やフォーマットなどを行うこともできます。

また、日付を文字列で表現する場合は、```format```関数を使うことで簡単にフォーマットすることができます。例えば、以下のように文字列を日付に変換することができます。

```Clojure
(def date-str "2019-10-01")
(format "yyyy-MM-dd" date-str) ;=> #object[java.time.LocalDate 0x81d022f 2019-10-01]
```

さらに、日付の計算を行う場合は、```with-interval```関数を使うことで便利に計算することができます。例えば、以下のように日付に14日を足すことができます。

```Clojure
(with-interval + (local-date 2019 10 01) 14) ;=> #object[java.time.LocalDate 0x16a9c49 2019-10-15]
```

## 参考リンク
- [Clojure中心 - 日付と時刻](https://clojure.or.tk/clojure/ref/clojure.core/date-time.html)
- [Java 8のjava.timeパッケージで日付、時間、および期間を操作する方法](https://www.baeldung.com/java-8-date-time-intro)