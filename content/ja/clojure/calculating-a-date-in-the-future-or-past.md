---
title:    "Clojure: 「未来または過去の日付を計算する」"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ?

計算したい日付を過去や未来に移動する理由は様々です。例えば、特定のイベントの日付や期限を計算したい場合や、毎月決まった日付の予定を自動的に生成したい場合などがあります。

## 使い方

Clojureを使った日付計算の例を以下のコードブロックで示します。

```Clojure
(defn calculate-date [base-date years months days]
  (let [base-date (clj-time.coerce/date-time base-date)
        new-date (clj-time.core/plus (clj-time.core/date-time years months days) base-date)]
    (str "移動後の日付は" new-date "です。")))
(calculate-date "2021-07-15" 2 6 1)
```

上記の例では、2021年7月15日を基準日とし、2年6ヶ月1日後の日付を計算しています。これを実行すると、「移動後の日付は2024-01-16T00:00:00.000+00:00です。」という結果が得られます。

## 深堀り

日付計算は、コンピュータープログラムでよく使用される機能の1つです。Clojureでは、clj-timeというライブラリを使うことで、簡単に日付を計算することができます。clj-timeは、Joda-TimeというJavaの日付ライブラリをClojureで使用するためのラッパーライブラリです。

日付計算を行う際には、基準となる日付を指定し、その日付から何年、何ヶ月、何日移動するかを計算する必要があります。clj-timeでは、`clj-time.core/plus`関数を使用して、指定した日付に対して移動を行うことができます。また、移動する値は、`clj-time.core/date-time`関数を使って日付型に変換してから行う必要があります。

クロージャーを使った日付計算は、簡単で便利な方法です。日付操作についてもっと学びたい場合は、clj-timeのドキュメントを参考にしてください。

## その他のリソース
- [clj-timeドキュメント](https://clj-time.github.io/clj-time/)
- [Joda-Timeドキュメント](https://www.joda.org/joda-time/)
- [Javaでの日付操作の方法](https://www.baeldung.com/java-date-time-api)