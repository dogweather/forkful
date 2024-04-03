---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:59.349728-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.571081-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\
  \u3059\u308B\u3053\u3068\u306F\u3001\u65E5\u4ED8\u3084\u6642\u9593\u306E\u6587\u5B57\
  \u5217\u8868\u73FE\u3092\u3088\u308A\u4F7F\u3044\u3084\u3059\u3044\u5F62\u5F0F\uFF08\
  \u4F8B\u3048\u3070\u3001Clojure\u306EDateTime\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \uFF09\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\
  \u3002\u3053\u306E\u30D7\u30ED\u30BB\u30B9\u306F\u3001\u30C7\u30FC\u30BF\u51E6\u7406\
  \u3001\u30ED\u30AE\u30F3\u30B0\u3001\u307E\u305F\u306F\u6642\u9593\u7684\u306A\u30C7\
  \u30FC\u30BF\u3092\u64CD\u4F5C\u3059\u308B\u4EFB\u610F\u306E\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u57FA\u672C\u7684\u3067\u3042\u308A\
  \u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u65E5\u4ED8\u306B\u95A2\u3057\u3066\
  \u52B9\u7387\u7684\u306B\u64CD\u4F5C\u3001\u6BD4\u8F03\u3001\u307E\u305F\u306F\u64CD\
  \u4F5C\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3067\u304D\u308B\u3088\u3046\u306B\u3057\
  \u307E\u3059\u3002."
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
