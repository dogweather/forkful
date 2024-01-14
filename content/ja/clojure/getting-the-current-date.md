---
title:    "Clojure: 現在の日付を取得する"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

今日は、Clojureプログラミング言語を学び、現在の日付を取得する方法についてお話しします。Clojureは柔軟で強力なプログラミング言語であり、日付や時間に関する処理を簡単に行うことができます。これからご紹介する方法を使えば、日付の取得が簡単にできるようになるでしょう。

## なぜ日付を取得する必要があるのか

現在の日付は、多くのアプリケーションやプログラムで必要とされる重要な情報です。例えば、日誌アプリケーションを作る場合、ユーザーにとって重要なことは、その日に書いた日記の日付です。また、取引履歴を管理するプログラムで、取引が行われた日付を確認することも重要です。そのような場合に日付を取得する必要があります。

## 日付を取得する方法

まずはじめに、Clojureで現在の日付を取得する方法を見てみましょう。以下のコードを試してみてください。

```Clojure
;; 現在の日付を取得する
(defn get-current-date []
  (java.time.LocalDate/now))
```

上記のコードでは、`java.time.LocalDate`クラスの`now`メソッドを使って現在の日付を取得しています。`defn`は、関数を定義するマクロです。

次に、現在の日時を取得する方法も見てみましょう。

```Clojure
;; 現在の日時を取得する
(defn get-current-date-time []
  (java.time.LocalDateTime/now))
```

上記のコードのように、`java.time.LocalDateTime`クラスの`now`メソッドを使うことで、現在の日時を取得することができます。

## より詳細な日付の取得方法

Clojureの`java.time`パッケージには、さまざまなクラスがあり、日付や時間に関する処理を行うことができます。例えば、`java.time.LocalDate`クラスでは、指定した年月日を指定することで、その日の曜日や月の日数などを取得することができます。

```Clojure
;; 任意の年月日の情報を取得する
(defn get-date-info [year month day]
  (let [date (java.time.LocalDate/of year month day)]
    {:day-of-week (.getDayOfWeek date)
     :day-of-month (.getDayOfMonth date)
     :month (.getMonth date)
     :year (.getYear date)}))

(get-date-info 2021 10 10)
;; => {:day-of-week #java.time.DayOfWeek/SUNDAY,
;;     :day-of-month 10,
;;     :month #java.time.Month/OCTOBER,
;;     :year 2021}
```

以上のように、Clojureの`java.time`パッケージを使うことで、より詳細な日付の取得が可能です。

## まとめ

今回は、Clojureを使って現在の日付を取得する方法についてご紹介しました。日付や時間に関する処理を行う際には、ぜひこの記事の内容を参考にしてみてください。また、他にもClojureにはさまざまなパッケージやライブラリがありますので、さらに学習をすすめてみてください。

## 関連記事

- [Clojureの日付と時間を扱うためのライブラリ一覧](https://qiita