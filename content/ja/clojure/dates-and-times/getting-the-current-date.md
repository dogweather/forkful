---
title:                "現在の日付の取得"
aliases:
- /ja/clojure/getting-the-current-date.md
date:                  2024-02-03T19:09:20.548491-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
プログラムにおいて現在の日付を取得することは、ログ取り、イベントのタイムスタンプ記録、タスクのスケジュール設定を含め、様々な理由から極めて重要です。JVM上のLisp方言であるClojureでは、このタスクはJavaの相互運用能力を活用しており、豊富なJava Date-Time APIへの直接アクセスを可能にしています。

## どのように：

### Java相互運用を使用して
ClojureのJavaとのシームレスな相互運用性により、Java Date-Time APIに直接アクセスすることができます。ここでは現在の日付を取得する方法を示します：

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; サンプル出力
(get-current-date) ; "2023-04-15"
```

### clj-timeライブラリを使用して
よりClojureらしい解決方法を求める場合は、Joda-Timeをラップした`clj-time`ライブラリが選択肢になりますが、ほとんどの新しいプロジェクトでは、組み込みのJava 8 Date-Time APIの使用が推奨されます。それでも`clj-time`を好む、または必要とする場合は：

まず、プロジェクトの依存関係に`clj-time`を追加します。`project.clj`に次のように記載します：

```clojure
[clj-time "0.15.2"]
```

その後、現在の日付を取得するためにそれを使います：

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; サンプル出力
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

これらの方法はどちらも、基盤となるJavaプラットフォームの力を活用するか、Clojure特有のライブラリの利便性を使用して、Clojureで現在の日付を迅速かつ効果的に取得する方法を提供します。
