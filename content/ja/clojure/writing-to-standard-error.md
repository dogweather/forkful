---
title:                "「標準エラーに書き込む」"
html_title:           "Clojure: 「標準エラーに書き込む」"
simple_title:         "「標準エラーに書き込む」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーに書き込むことが重要な理由は、プログラミング中に発生したエラーを特定し解決するためです。エラーを標準エラーに書き込むことで、より詳細な情報を得ることができ、問題をより早く解決することができます。

## 使い方

エラーを標準エラーに書き込むには、```(println)``` を使います。例えば、 ```(println "エラーが発生しました")``` と書くと、標準エラーに "エラーが発生しました" というメッセージが出力されます。

もしデバッグのためにエラーの詳細な情報を見たい場合は、 ```(println (str "エラーが発生しました:" err))``` と書くことで、エラーの内容が標準エラーに出力されます。

## 詳細を知る

標準エラーに書き込むことで、プログラミング中のエラーをより効率的に解決することができます。また、 ```(eprintln)``` という関数を使うことで、標準エラーに書き込んだ情報を赤色で表示することができるので、より目立たせることができます。

## 参考

- [標準エラーに出力する関数 - Clojureドキュメント](https://clojuredocs.org/clojure.core/eprintln)
- [例外処理と標準エラーに書き込む方法 - Kageno Blog](https://kageno-blog.com/programming/clojure/exception-handling)