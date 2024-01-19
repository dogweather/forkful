---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？
HTTPリクエストの送信は、コンピュータからWebサーバーに特定の情報を要求するための手段です。プログラマーはこれを用いてウェブサイトからデータを取得したり、APIと情報を交換したりします。

## 方法：
ClojureでのHTTPリクエストの送信は、`clj-http` ライブラリを用いて容易に行うことができます。まずはこのライブラリをあなたのプロジェクトに追加しましょう。

```Clojure
:dependencies [[clj-http "3.12.3"]]
```

次に、GETリクエストの例を見てみましょう。

```Clojure
(require '[clj-http.client :as client])

(defn fetch-data []
  (let [response (client/get "https://api.example.com/data")]
    (:body response)))
```

この関数は、指定されたURLからデータを取得します。応答の本文部分は `:body` キーでアクセスできます。

## ディープダイブ：
HTTPリクエストは、1991年に登場したHTTPプロトコルの基本的な機能です。これ以前は、情報の交換はFTPやTelnetなどの他のプロトコルを介して行われていました。

送信方法はいくつかあります。POST、GET、PUT、DELETEなどのHTTPメソッドがあり、それぞれ異なる用途で使用されます。たとえば、GETはサーバーからデータを取得するために使用し、POSTは新たにデータを作成したり、既存のデータを更新したりするために使用します。

Clojureの `clj-http` ライブラリは、JavaのApache HttpComponentsをラップします。これによりプログラマーは複雑なJava APIを直接扱う必要なく、HTTPリクエストを簡単に送信できます。

## 参照情報：
HTTPプロトコルについての詳細は、以下のリンクを参照してください：

1. [Mozilla HTTP Guide](https://developer.mozilla.org/en-US/docs/Web/HTTP)
2. [Clojure clj-http documentation](https://github.com/dakrone/clj-http)
3. [Apache HttpComponents](https://hc.apache.org/)