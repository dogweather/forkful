---
title:                "「現在の日付を取得する」"
html_title:           "Clojure: 「現在の日付を取得する」"
simple_title:         "「現在の日付を取得する」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

今日の日付を取得することは、プログラムにおいて非常に一般的なタスクです。なぜなら、多くのアプリケーションでは、ユーザーが現在の日付を知る必要があるからです。例えば、予定表やタイムスタンプ付きのファイルを作成する際には、常に現在の日付が必要になります。

## 使い方：

```Clojure
(def current-date (java.util.Date.))
(println current-date)
```

```
#Sat Dec 11 14:22:23 JST 2021
```

上記の例では、まずClojureの組み込みのjava.util.Dateを使用して現在の日付を取得します。次に、printlnを使用してその日付をコンソールにプリントします。

## 詳細：

日付の取得は、コンピュータプログラミングにおいて非常に古くから行われてきた作業です。最も古典的な方法は、システムクロックから直接取得する方法です。しかし、現代では、ネットワーク経由で日付を取得することがほとんどです。また、Clojureには、Java Calendar APIという、日付関連のより高度な操作をサポートするライブラリがあります。

## 関連情報：

- [Java Calendar API](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java Date and Time API](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)