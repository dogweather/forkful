---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 何となぜ？

ウェブページのダウンロードとは、ウェブサーバからクライアントマシンへのファイルのコピーです。プログラマはこれを行うことで、オフラインでの閲覧、データ分析、ウェブスクレイピングなどを可能にします。

## 手順は以下の通り：

以下に、Clojureを使用してウェブページをダウンロードする例を示します。

```clojure
(ns my-app.core
    (:require [clj-http.client :as client]))

(defn download-page [url]
  (let [response (client/get url)]
    (if (= 200 (:status response))
      (:body response)
      (throw (Exception. "Failed to download")))))
```

この関数は、引数としてURLを受け取り、そのURLの内容を返します。ダウンロードが失敗すると例外をスローします。

## ディープダイブ:

久しい過去から、プログラマーは独自のHTMLパーサーやダウンローダーを作成してきました。しかし、これらのツールは時間とともに同様の過ちを繰り返し、ベストプラクティスはライブラリの形で確立されました。Clojureのclj-httpはその一つでHTTPリクエストの送受信を簡易化します。

ダウンロードする方法は他にもあります。例えば、java.net.URLや java.nio.fileを使用するなど。しかし、clj-httpはシンプルさと拡張性で優れています。

具体的な実装としては、clj-httpライブラリは内部でJavaのApache HttpClientライブラリを使用しています。これにより、高度なHTTP通信機能がClojureから直接利用可能になっています。

## また見てみましょう:

以下のリンクで更に詳細な情報を得ることができます：

- clj-httpリポジトリ（英語）: https://github.com/dakrone/clj-http
- Clojureでのウェブスクレイピングについての詳細なチュートリアル（英語）: https://realpython.com/python-web-scraping-practical-introduction/
- Apache HttpClientの公式ドキュメンテーション（英語）: https://hc.apache.org/httpcomponents-client-ga/index.html