---
title:                "ウェブページのダウンロード"
html_title:           "Clojure: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なに & なぜ？
Webページをダウンロードすることは、インターネットからローカルコンピューターにファイルをコピーすることを意味します。プログラマーは、Webページをダウンロードすることによって、そのページの情報を取得し、処理することができます。

## 方法：
```Clojure
(require '[clojure.java.io :as io])
(def url "https://www.example.com")
(io/copy (java.net.URL. url) (io/file "example.html"))
```
上記のコードはClojureを使用してWebページをダウンロードする方法の一例です。ダウンロードしたページは、"example.html"という名前のファイルとして保存されます。

## 深く掘り下げる
Webページをダウンロードする方法として、上記のコード以外にも様々な手法があります。その中でも最も一般的な方法は、HTTPリクエストを使用する方法です。また、ClojureにはHTTPリクエストをより簡単に扱うことができるライブラリがあります。

Clojureでは、java.net.URLクラスを使用することで、Webページをダウンロードすることができます。このクラスはJavaプログラミング言語の一部であり、Clojureでも使用することができます。

## 関連リンク
- [Clojureのjava.net.URLクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [HTTPリクエストを取り扱うためのClojureライブラリ「clj-http」](https://github.com/dakrone/clj-http)