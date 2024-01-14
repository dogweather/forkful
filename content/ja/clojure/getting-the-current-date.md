---
title:                "Clojure: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ？

現在の日付を取得する理由は何でしょうか？Clojureには、日付を取得するための便利な機能があります。例えば、今日の日付を使って特定の処理を実行したい場合や、ある日付が特定の範囲内にあるかどうかをチェックする必要がある場合などです。

## 方法

Clojureで現在の日付を取得するには、`java.time.LocalDate`を使います。以下のようにコードを書くことで、現在の日付を取得することができます。

```Clojure
(def today (java.time.LocalDate/now))
```

このコードを実行すると、以下のような出力が得られます。

```Clojure
#object[java.time.LocalDate "2020-06-08"]
```

また、時刻も含めたい場合は、`java.time.LocalDateTime`を使います。

```Clojure
(def current-time (java.time.LocalDateTime/now))
```

出力は以下のようになります。

```Clojure
#object[java.time.LocalDateTime "2020-06-08T15:45:28.182627"]
```

## 深堀り

Clojureでは、ローカルの日付や時刻だけでなく、他のタイムゾーンの日付や時刻も取得することができます。例えば、東京の現在の日付を取得するには、`withZone`関数を使います。

```Clojure
(def tokyo-time (java.time.LocalDateTime/now (java.time.ZoneId/of "Asia/Tokyo")))
```

出力は以下のようになります。

```Clojure
#object[java.time.LocalDateTime "2020-06-09T00:45:28.182627"]
```

さらに、日付や時刻のフォーマットも変更することができます。例えば、フォーマットを`yyyy-MM-dd`にするには、`format`関数を使います。

```Clojure
(def formatted-time (.format (java.time.DateTimeFormatter/ofPattern "yyyy-MM-dd") today))
```

出力は以下のようになります。

```Clojure
"2020-06-08"
```

## 他の関連記事

[Official Java documentation for LocalDate](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)

[Official Java documentation for LocalDateTime](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDateTime.html)

[How to get the current date and time in Clojure](https://scottshipp.com/how-to-get-the-current-date-and-time-in-clojure/)