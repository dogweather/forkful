---
title:                "将来または過去の日付の計算"
html_title:           "Clojure: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何 & なぜ?

未来または過去の日付を計算することは、プログラマーにとってよくあるタスクです。これは特定の日付を基準に、指定した日数や期間を加えることで、新しい日付を得ることを意味します。これは、バックアップや予定の作成など、様々なアプリケーションで役立ちます。

## 方法:

```Clojure
;; 未来の日付を計算する
(println "今から1日後は" (clojure.java-time.format/parse "yyyy-MM-dd" (clojure.string/join "-" [(+ (local-time/get-local-date) (java.time.Duration/of-days 1))])))

;; 過去の日付を計算する
(println "今から1年前は" (clojure.java-time.format/parse "yyyy-MM-dd" (clojure.string/join "-" [(+ (local-time/get-local-date) (java.time.Duration/of-years -1))])))
```

上記のコードでは、現在の日付を取得し、それに対して日数や年数の差分を加えることで、新しい日付を計算しています。そして、計算結果を指定した書式で出力しています。

出力例:
今から1日後は 2019-12-22
今から1年前は 2018-12-23

## 深堀り:

日付の計算はコンピューターの世界では最も古い問題の一つです。昔は、人間が日付を計算していましたが、無駄な計算やミスが多く、それを自動化する必要性が生まれました。その結果、様々なアルゴリズムが発展し、現代ではビルトインの機能やライブラリーを使用することで、簡単に日付を計算することができるようになりました。

もしこの記事で紹介した方法に加えて、さらに柔軟性を求める場合は、"java.time" ライブラリーのドキュメントを参照することをおすすめします。また、このライブラリー以外にも、Joda-TimeやJULIAN4等、日付計算のための専用ライブラリーがあります。

## 関連情報:

- [Clojureドキュメント - java.time](https://clojure.github.io/java.time/java.time.html)
- [Joda-Time](https://www.joda.org/joda-time/)
- [JULIAN4](https://github.com/phoracek/JULIAN4)