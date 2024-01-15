---
title:                "現在の日付を取得する"
html_title:           "Clojure: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
なぜ現在の日付を取得するのか？日付を取得することは、プログラミングにおいて非常に一般的なタスクです。例えば、特定の日付を必要とするスケジューリングや、ログに記録するなどの目的があります。プログラムが現在の日付を自動的に取得できるようにすることで、手動で日付を入力する手間を省くことができるため、効率的な作業を行うことができます。

## How To
### Clojureを使用した現在の日付の取得方法
```Clojure
(println (java.util.Date.))
```
出力:
```
Thu Feb 25 23:14:32 JST 2021
```

```Clojure
(println (java.time.LocalDate/now))
```
出力:
```
2021-02-25
```

### フォーマットを指定して日付を取得する方法
Clojureには、フォーマットを指定して日付を取得するための専用のライブラリであるclj-timeがあります。このライブラリを使うと、より詳細な日付の表現を得ることができます。例えば、"MM/dd/yyyy"のようなパターンを指定することで、日付を月/日/年のフォーマットで取得することができます。

```Clojure
(require '[clj-time.core :as t])

(println (t/format (t/now) "MM/dd/yyyy"))
```
出力:
```
02/25/2021
```

### 特定のタイムゾーンの日付を取得する方法
デフォルトでは、Clojureはシステムのタイムゾーンを使用して日付を取得します。しかし、必要に応じて特定のタイムゾーンの日付を取得することもできます。例えば、アメリカの太平洋標準時の日付を取得する場合は、Pacific/Aucklandというタイムゾーンを指定します。

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as tconv])

(println (t/format (t/now (tconv/to-time-zone "Pacific/Auckland")) "MM/dd/yyyy"))
```
出力:
```
02/26/2021
```

## Deep Dive
Clojureでは、日付を取得するためにJavaの標準ライブラリであるjava.util.Dateを使用することができます。また、clj-timeライブラリを使うことで、より柔軟なフォーマットやタイムゾーンの指定が可能となります。さらに、clj-timeはJoda-Timeというライブラリをベースにしており、さまざまな便利な関数を提供しています。

## See Also
- [Clojureで日付を扱う方法] (https://clojure.or.jp/timeline/)
- [clj-timeライブラリの詳細] (https://github.com/clj-time/clj-time)
- [Joda-Timeライブラリの詳細] (https://www.joda.org/joda-time/)