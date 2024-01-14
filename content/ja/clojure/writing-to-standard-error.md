---
title:                "Clojure: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラー出力に書き込むことの重要性は何ですか？プログラミングにおいて、標準エラー出力を理解することで、エラーの原因をすばやく特定し、修正することができます。

## やり方

標準エラー出力に書き込むには、`println`関数を使用します。例えば、次のコードがあります。

```Clojure
(defn divide [x y]
  (/ x y))

(try
  (divide 10 0)
  (catch Exception e
    (println "Error: " (:cause e))))
```

上記のコードでは、10を0で割ることを試みています。しかし、0で割り切れないため、標準エラー出力に"Error: "というメッセージが出力されます。

## 深堀り

標準エラー出力は、標準出力とは異なり、ユーザーに対しては表示されず、コンソールにのみ出力されます。これは、エラーメッセージを特別扱いするためです。また、コマンドラインプログラムでは、標準エラー出力を使用することで、システムログにエラーメッセージを出力することができます。

## 他に見る

[標準エラー出力(Documentation)](https://clojuredocs.org/clojure.core/println)