---
aliases:
- /ja/clojure/downloading-a-web-page/
date: 2024-01-20 17:43:49.443110-07:00
description: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\
  \u308B\u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u30DA\
  \u30FC\u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3057\u3001\u81EA\u5206\u306E\u30C7\
  \u30D0\u30A4\u30B9\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u5206\u6790\u3084\u30D0\u30C3\
  \u30AF\u30A2\u30C3\u30D7\u4F5C\u6210\u3001\u307E\u305F\u306F\u30AA\u30D5\u30E9\u30A4\
  \u30F3\u3067\u306E\u95B2\u89A7\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.602768
model: gpt-4-1106-preview
summary: "Web\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9\u3059\u308B\
  \u3068\u306F\u3001\u30A4\u30F3\u30BF\u30FC\u30CD\u30C3\u30C8\u304B\u3089\u30DA\u30FC\
  \u30B8\u306E\u5185\u5BB9\u3092\u53D6\u5F97\u3057\u3001\u81EA\u5206\u306E\u30C7\u30D0\
  \u30A4\u30B9\u306B\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u5206\u6790\u3084\u30D0\u30C3\u30AF\
  \u30A2\u30C3\u30D7\u4F5C\u6210\u3001\u307E\u305F\u306F\u30AA\u30D5\u30E9\u30A4\u30F3\
  \u3067\u306E\u95B2\u89A7\u306A\u3069\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
Webページをダウンロードするとは、インターネットからページの内容を取得し、自分のデバイスに保存することです。プログラマーはデータ分析やバックアップ作成、またはオフラインでの閲覧などのためにこれを行います。

## How to: (方法)
Clojureでは、`clj-http`ライブラリを使って簡単にWebページをダウンロードできます。

```Clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

(def page-content (download-page "http://example.com"))

(println (:body page-content))
```

これで`http://example.com`の内容がコンソールに表示されます。

## Deep Dive (深掘り)
Webページのダウンロードは、1990年代のWebの誕生から重要な役割を担っています。`clj-http`には独自のエージェントのような多くのオプションがあり、SSL、ヘッダー情報のカスタマイズ、またはHTTPメソッドの変更が可能です。代替手段としては、Javaの組み込みライブラリなども利用できますが、`clj-http`はClojureのスタイルに適しており、より簡潔です。このライブラリは内部ではApache HttpClientを使用しており、複雑なHTTP要求にも柔軟に対応できます。

## See Also (参照)
- clj-http GitHub レポジトリ: https://github.com/dakrone/clj-http
- Clojure公式ドキュメンテーション: https://clojure.org/
- Apache HttpClient: http://hc.apache.org/httpcomponents-client-ga/
