---
date: 2024-01-20 17:32:46.534902-07:00
description: "\u65E5\u4ED8\u306E\u6BD4\u8F03\u306F\u4E8C\u3064\u306E\u65E5\u4ED8\u3092\
  \u898B\u3066\u3001\u3069\u3061\u3089\u304C\u904E\u53BB\u304B\u672A\u6765\u304B\u3001\
  \u3042\u308B\u3044\u306F\u540C\u3058\u304B\u5224\u5B9A\u3059\u308B\u30D7\u30ED\u30BB\
  \u30B9\u3067\u3059\u3002\u65E5\u4ED8\u3092\u30BD\u30FC\u30C8\u3057\u305F\u308A\u3001\
  \u671F\u9593\u3092\u8A08\u7B97\u3057\u305F\u308A\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\
  \u30EA\u30F3\u30B0\u306E\u30ED\u30B8\u30C3\u30AF\u3092\u5B9F\u88C5\u3059\u308B\u969B\
  \u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u3088\u304F\
  \u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.726272-07:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u306E\u6BD4\u8F03\u306F\u4E8C\u3064\u306E\u65E5\u4ED8\u3092\
  \u898B\u3066\u3001\u3069\u3061\u3089\u304C\u904E\u53BB\u304B\u672A\u6765\u304B\u3001\
  \u3042\u308B\u3044\u306F\u540C\u3058\u304B\u5224\u5B9A\u3059\u308B\u30D7\u30ED\u30BB\
  \u30B9\u3067\u3059\u3002\u65E5\u4ED8\u3092\u30BD\u30FC\u30C8\u3057\u305F\u308A\u3001\
  \u671F\u9593\u3092\u8A08\u7B97\u3057\u305F\u308A\u3001\u30B9\u30B1\u30B8\u30E5\u30FC\
  \u30EA\u30F3\u30B0\u306E\u30ED\u30B8\u30C3\u30AF\u3092\u5B9F\u88C5\u3059\u308B\u969B\
  \u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u3088\u304F\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付の比較は二つの日付を見て、どちらが過去か未来か、あるいは同じか判定するプロセスです。日付をソートしたり、期間を計算したり、スケジューリングのロジックを実装する際に、プログラマーはこれをよく行います。

## How to: (方法)
```Clojure
;; 日付ライブラリをインポート
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])

;; 日付を作成
(def date1 (coerce/to-date-time "2023-02-20T00:00:00.000Z"))
(def date2 (coerce/to-date-time "2023-02-21T00:00:00.000Z"))

;; 日付を比較
;; date1がdate2よりも前か？
(time/before? date1 date2)   ; => true

;; date1とdate2が同じ日付か？
(time/equal? date1 date2)    ; => false

;; date1がdate2よりも後か？
(time/after? date1 date2)    ; => false
```

## Deep Dive (深掘り)
日付の比較はJavaの`Date`クラスから始まりました。ClojureはJavaプラットフォーム上で動きますから、Javaのライブラリが使えます。それに `clj-time` ライブラリはJoda-Timeに基づいていて、より扱いやすいインターフェースを提供しています。違う方法もありますが、`clj-time` はClojureコミュニティで広く使われています。もし纏める必要があるなら、Java 8以降の`java.time`パッケージを使うという選択肢もありますが、Clojureとの統合性では `clj-time` の方が優れていると考えられます。

## See Also (関連情報)
- 完全な `clj-time` ドキュメンテーション: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Javaの日付と時刻APIガイド: [https://docs.oracle.com/javase/tutorial/datetime/](https://docs.oracle.com/javase/tutorial/datetime/)
- Clojure公式サイト: [https://clojure.org/](https://clojure.org/)
