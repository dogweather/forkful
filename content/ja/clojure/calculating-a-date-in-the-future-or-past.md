---
title:    "Clojure: 未来または過去の日時の計算"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算する必要があるのでしょうか？その答えは簡単です。現在の日付を基準にして、将来の計画や過去のイベントを管理したいからです。Clojureを使えば、このような計算を簡単に行うことができます。

## 方法

Clojureで日付を計算するには、`java.time`ライブラリを使います。まずはライブラリをインポートしましょう。

```Clojure
(ns date-calc
  (:import (java.time LocalDate)
           (java.time.temporal ChronoUnit)))
```

次に、計算したい日付を定義します。例えば、今日の日付を`today`変数に保存します。

```Clojure
(def today (LocalDate/now))
```

未来の日付を計算するには、`LocalDate#plus`メソッドを使います。引数には単位と数値を指定します。例えば、明日の日付を計算するには、以下のようにします。

```Clojure
(def tomorrow (.plus today 1 ChronoUnit/DAYS))
```

同様に、過去の日付を計算するには`LocalDate#minus`メソッドを使います。例えば、1週間前の日付を計算するには、以下のようにします。

```Clojure
(def last-week (.minus today 1 ChronoUnit/WEEKS))
```

計算した日付を使って何か処理をする場合、`LocalDate#format`メソッドを使って日付を指定した形式に変換することができます。たとえば、`yyyy/MM/dd`の形式に変換したい場合、以下のようにします。

```Clojure
(def formatted (str (.format tomorrow (java.time.format.DateTimeFormatter/ofPattern "yyyy/MM/dd"))))
```

以上で基本的な日付の計算ができるようになりました。

## ディープダイブ

日付を計算するために、より詳細なオプションを使いたい場合もあります。例えば、特定の月や年を指定して計算したい場合、`LocalDate#withMonth`や`LocalDate#withYear`メソッドを使うことができます。

また、時刻も含めたい場合は`LocalDateTime`クラスを使います。さらに、過去や未来の日時を計算する場合は`LocalDateTime#minus`や`LocalDateTime#plus`メソッドを使います。

さらに詳しい情報や細かい使用法は、[公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)を参照してください。

## 参考リンク

- https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html
- https://github.com/clojure/clojure.java-time