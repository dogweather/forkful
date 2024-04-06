---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:20.548491-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Clojure\u306EJava\u3068\u306E\u30B7\
  \u30FC\u30E0\u30EC\u30B9\u306A\u76F8\u4E92\u904B\u7528\u6027\u306B\u3088\u308A\u3001\
  Java Date-Time API\u306B\u76F4\u63A5\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-04-05T21:53:42.519254-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
