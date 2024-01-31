---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:14:02.807869-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を取得するのは、今日の日付や時刻を知るプロセスです。プログラマはログ、タイムスタンプ、期限など、システムの日時に敏感な機能を実装するためにこれを行います。

## How to: (方法)
Clojureで現在の日付を取得する最も一般的な方法は、Javaの `java.util.Date` クラスと `clj-time` ライブラリを使うことです。ここでは両方の方法を見てみましょう。

まずは `java.util.Date`:

```Clojure
(import 'java.util.Date)

(defn current-date []
  (str (Date.)))

(println (current-date)) ; => "Wed Mar 15 20:41:02 JST 2023"
```

次に `clj-time` を使った方法です:

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

(defn current-date-clj-time []
  (f/unparse (f/formatters :basic-date-time) (t/now)))

(println (current-date-clj-time)) ; => "20230315T204102.000Z"
```
出力は異なる形式になります。

## Deep Dive (深掘り)
歴史的に、ClojureはJavaのライブラリを利用することで日付と時刻を扱ってきました。`java.util.Date`はJavaの初期から存在しますが、デザイン上の問題点があり、2008年のJava 8で`java.time`パッケージに一新されました。Clojureプログラマは今でも`java.util.Date`を使うことができますが、`java.time`や`clj-time`といった、より現代的なライブラリの使用が推奨されます。

`clj-time`はJoda-TimeをClojureに適合させたライブラリであり、取扱が容易で機能も豊富です。しかし、最新のJavaプラットフォームを使っているならば、Joda-Timeと`clj-time`は`java.time`ライブラリの後継者として段階的に廃止されるべきです。

## See Also (関連情報)
- Clojure公式ドキュメント: https://clojure.org/
- `clj-time` GitHubページ: https://github.com/clj-time/clj-time
- Java 8 `java.time` ドキュメント: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- Joda-Time公式ウェブサイト: https://www.joda.org/joda-time/
