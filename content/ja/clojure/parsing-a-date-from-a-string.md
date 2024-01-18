---
title:                "文字列から日付を解析する"
html_title:           "Clojure: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## いつ&どうして？
日付を文字列から解析するとは、文字列から日付を取得することです。プログラマーがこれをする理由は、日付をプログラムで使用する必要があるからです。

## 方法：
Clojureの```parse```関数を使用することで、文字列から日付を解析することができます。例えば、```(parse "2020/05/15")```と入力することで、日付オブジェクトを取得することができます。

## 深いダイブ：
文字列から日付を解析することは、プログラミングにおいて非常に重要です。過去には、日付を表す文字列を各プログラミング言語で異なる方法で扱う必要がありましたが、Clojureでは統一的な方法で解析することができます。また、他の方法として、正規表現を使用する方法もあります。Clojureの```parse```関数は、内部的には正規表現を使用しています。

## 関連リンク：
- Clojureの日付・時刻処理に関するドキュメント：https://clojuredocs.org/clojure.java-time/parse
- 正規表現による文字列の解析方法：https://qiita.com/takeyuweb/items/bda8e4c70f71702064d0