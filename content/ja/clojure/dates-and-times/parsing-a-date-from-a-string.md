---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:59.349728-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u306FJVM\u8A00\u8A9E\u3067\u3042\u308B\u305F\
  \u3081\u3001Java\u306E\u65E5\u4ED8\u3068\u6642\u9593\u306E\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u76F4\u63A5\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u307E\u305A\u3001\
  \u7D44\u307F\u8FBC\u307F\u306EJava\u76F8\u4E92\u904B\u7528\u3092\u59CB\u3081\u306B\
  \u3057\u3066\u3001\u305D\u306E\u5F8C\u3001\u3088\u308A\u30A4\u30C7\u30A3\u30AA\u30DE\
  \u30C6\u30A3\u30C3\u30AF\u306AClojure\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\u30F3\u306B\
  \u5BFE\u3057\u3066\u4EBA\u6C17\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3001clj-time\u306E\u5229\u7528\u65B9\u6CD5\u3092\u63A2\
  \u308A\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:49.899866-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Clojure\u306FJVM\u8A00\u8A9E\u3067\u3042\u308B\u305F\u3081\
  \u3001Java\u306E\u65E5\u4ED8\u3068\u6642\u9593\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u76F4\u63A5\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002\u307E\u305A\u3001\u7D44\
  \u307F\u8FBC\u307F\u306EJava\u76F8\u4E92\u904B\u7528\u3092\u59CB\u3081\u306B\u3057\
  \u3066\u3001\u305D\u306E\u5F8C\u3001\u3088\u308A\u30A4\u30C7\u30A3\u30AA\u30DE\u30C6\
  \u30A3\u30C3\u30AF\u306AClojure\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\u30F3\u306B\u5BFE\
  \u3057\u3066\u4EBA\u6C17\u306E\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3001clj-time\u306E\u5229\u7528\u65B9\u6CD5\u3092\u63A2\u308A\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法：
ClojureはJVM言語であるため、Javaの日付と時間のライブラリを直接使用できます。まず、組み込みのJava相互運用を始めにして、その後、よりイディオマティックなClojureソリューションに対して人気のサードパーティライブラリ、clj-timeの利用方法を探ります。

### Java相互運用を使用する
Clojureは、文字列から日付を解析するためにJavaの`java.time.LocalDate`を直接活用できます：
```clojure
(require '[clojure.java.io :as io])

; Java相互運用を使用して日付を解析する
(let [date-str "2023-04-01"
      date (java.time.LocalDate/parse date-str)]
  (println date))
; 出力: 2023-04-01
```

### clj-timeを使用する
日付と時間を扱うためのよりイディオマティックなClojureライブラリである`clj-time`があります。これは、日付と時間の操作のための包括的なライブラリであるJoda-Timeをラップしています。まず、`clj-time`をプロジェクトの依存関係に追加する必要があります。ここでは、`clj-time`を使用して日付文字列を解析する方法を示します：

```clojure
; プロジェクトのproject.cljの :dependencies に[clj-time "0.15.2"]を追加することを確認してください

(require '[clj-time.format :as fmt]
         '[clj-time.core :as time])

; フォーマッタを定義する
(let [formatter (fmt/formatter "yyyy-MM-dd")
      date-str "2023-04-01"
      parsed-date (fmt/parse formatter date-str)]
  (println parsed-date))
; 出力: #object[org.joda.time.DateTime 0x76eccb5d "2023-04-01T00:00:00.000Z"]
```

これらの例は、基本的な日付解析を示しています。両方の方法は有用ですが、`clj-time`はよりClojure中心のアプローチを提供でき、複雑な要件に対する追加機能を提供することができます。
