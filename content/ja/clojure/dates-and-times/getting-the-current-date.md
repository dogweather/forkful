---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:20.548491-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u304A\u3044\u3066\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u30ED\u30B0\u53D6\
  \u308A\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\
  \u8A18\u9332\u3001\u30BF\u30B9\u30AF\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u8A2D\
  \u5B9A\u3092\u542B\u3081\u3001\u69D8\u3005\u306A\u7406\u7531\u304B\u3089\u6975\u3081\
  \u3066\u91CD\u8981\u3067\u3059\u3002JVM\u4E0A\u306ELisp\u65B9\u8A00\u3067\u3042\u308B\
  Clojure\u3067\u306F\u3001\u3053\u306E\u30BF\u30B9\u30AF\u306FJava\u306E\u76F8\u4E92\
  \u904B\u7528\u80FD\u529B\u3092\u6D3B\u7528\u3057\u3066\u304A\u308A\u3001\u8C4A\u5BCC\
  \u306AJava Date-Time\u2026"
lastmod: '2024-03-13T22:44:41.572405-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u304A\u3044\u3066\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u306F\u3001\u30ED\u30B0\u53D6\
  \u308A\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D7\
  \u8A18\u9332\u3001\u30BF\u30B9\u30AF\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\u8A2D\
  \u5B9A\u3092\u542B\u3081\u3001\u69D8\u3005\u306A\u7406\u7531\u304B\u3089\u6975\u3081\
  \u3066\u91CD\u8981\u3067\u3059\u3002JVM\u4E0A\u306ELisp\u65B9\u8A00\u3067\u3042\u308B\
  Clojure\u3067\u306F\u3001\u3053\u306E\u30BF\u30B9\u30AF\u306FJava\u306E\u76F8\u4E92\
  \u904B\u7528\u80FD\u529B\u3092\u6D3B\u7528\u3057\u3066\u304A\u308A\u3001\u8C4A\u5BCC\
  \u306AJava Date-Time API\u3078\u306E\u76F4\u63A5\u30A2\u30AF\u30BB\u30B9\u3092\u53EF\
  \u80FD\u306B\u3057\u3066\u3044\u307E\u3059\u3002."
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
