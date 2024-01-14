---
title:                "Clojure: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページのダウンロードをフレッシュで、クリーンな状態で保存することは便利です。例えば、ウェブスクレイピングやデータ収集などのプロジェクトにとって有効であり、より正確なデータを収集することができます。

## 手順

```Clojure
(ns download-page.core
  (:require [clj-http.client :as client]
            [clojure.java.io :as io]))

(defn download-page [url destination]
  (let [response (client/get url)
        output (str destination ".html")]
    (spit output (:body response))))

;; 実行例
(download-page "https://example.com" "/home/user/downloads/example")

;; 実行後、/home/user/downloads に example.html が保存されます。
```

## 詳細解説

ウェブページのダウンロードには、多くのクライアントライブラリが利用できますが、今回はclj-httpライブラリを使用しました。`client/get`を使用することで、指定したURLからHTMLのレスポンスを取得することができます。また、`spit`関数を使うことで、取得したHTMLを指定した場所に保存することができます。

## お取り寄せ

- [clj-httpライブラリ](https://github.com/dakrone/clj-http)
- [Clojureのプロジェクト設定](https://clojure.org/guides/deps_and_cli)
- [クロージャーの構文ガイド](https://clojure.org/guides/learn/syntax)



## 参考リンク

- [Webスクレイピング入門](https://qiita.com/okoppe8/items/7bd9a0a4d6e6f1bd4cda)
- [Clojure for the Brave and True](https://www.braveclojure.com/)