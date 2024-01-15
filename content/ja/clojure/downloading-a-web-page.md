---
title:                "Webページをダウンロードする"
html_title:           "Clojure: Webページをダウンロードする"
simple_title:         "Webページをダウンロードする"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

Webページをダウンロードする理由は、そのコンテンツをオフラインで閲覧することができるからです。また、Webスクレイピングやデータ収集などの目的にも使われます。

## 方法

Webページをダウンロードするには、Clojureの```clj-http```ライブラリを使用します。まずはClojureプロジェクトを作成し、```project.clj```ファイルに```[clj-http "3.11.0"]```を依存関係として追加します。次に、```core.clj```ファイルを作成し、以下のコードを追加します。

```Clojure
(ns my-project.core
  (:require [clj-http.client :as client]))

(defn download-page [url]
  (let [response (client/get url)
        status (:status response)
        body (:body response)]
    (if (= 200 status)
      body
      (throw (Exception. (str "Error downloading page: " status))))))

;; ダウンロードしたいWebページのURLを引数として渡します
(download-page "https://www.example.com")
```

このコードでは、```download-page```関数を定義し、指定したURLからページをダウンロードします。ダウンロードが成功した場合は、ページのHTMLコンテンツが返されます。もしダウンロードが失敗した場合は、エラーメッセージをスローします。

## ディープダイブ

```clj-http```の詳細な使い方や設定方法については公式ドキュメントを参照してください。また、Webページのダウンロード以外にも、リクエストヘッダーの設定やクッキーの管理なども可能です。

## 関連リンク

- ```clj-http```公式ドキュメント: https://github.com/dakrone/clj-http
- WebスクレイピングのためのClojureライブラリ「Enlive」: https://github.com/cgrand/enlive
- フロントエンド開発に便利なClojureScript: https://clojurescript.org/