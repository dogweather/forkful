---
title:                "現在の日付の取得"
html_title:           "Bash: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何となぜ？

現在の日付を取得することは、実行時の日付を知るためのプログラミングタスクです。ログのタイムスタンプ、日付計算、データの時系列分析などで必要とされます。

## 方法：

Clojureでは、`java.util.Date`クラスを用いて現在の日付を取得することが可します。例：

```Clojure
(import 'java.util.Date)

(defn current-date []
  (Date.))

(current-date)
```

出力例：
    
```Clojure
#inst "2021-10-03T12:05:44.786-00:00"
```

## 深堀り：

1. 歴史的背景： ClojureはJavaの上に構築されているため、Javaの`java.util.Date`を使用するのが一般的です。

2. 代替案： Clojureでは、`clj-time`ライブラリも用いて日付を扱うことができます。ほとんどの場合、このライブラリを利用する方が便利で有用です。

3. 実行詳細： `Date.`は、コンストラクタのsyntactic sugarで、現在の時刻で新しい`Date`オブジェクトを作成します。

## 関連資料：

1. Clojureの公式ドキュメンテーション: https://clojure.org/
2. `clj-time`ライブラリのGitHubページ: https://github.com/clj-time/clj-time
3. Javaの公式Dateクラスドキュメンテーション: https://docs.oracle.com/javase/8/docs/api/java/util/Date.html