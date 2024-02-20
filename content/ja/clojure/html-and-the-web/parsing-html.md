---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:48.803294-07:00
description: "Clojure\u3067\u306EHTML\u89E3\u6790\u306F\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u3067HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u60C5\u5831\u3092\
  \u62BD\u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3063\u3066\u3001\u30A6\
  \u30A7\u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u305F\
  \u308A\u3001\u52D5\u7684\u306B\u64CD\u4F5C\u3057\u305F\u308A\u3001\u76E3\u8996\u3057\
  \u305F\u308A\u3059\u308B\u3053\u3068\u3067\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\
  \u5316\u3057\u305F\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\
  \u30C7\u30FC\u30BF\u3092\u4F9B\u7D66\u3057\u305F\u308A\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.829107
model: gpt-4-0125-preview
summary: "Clojure\u3067\u306EHTML\u89E3\u6790\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u3067HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u60C5\u5831\u3092\u62BD\
  \u51FA\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u884C\u3063\u3066\u3001\u30A6\u30A7\
  \u30D6\u30B3\u30F3\u30C6\u30F3\u30C4\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u308A\
  \u3001\u52D5\u7684\u306B\u64CD\u4F5C\u3057\u305F\u308A\u3001\u76E3\u8996\u3057\u305F\
  \u308A\u3059\u308B\u3053\u3068\u3067\u3001\u30BF\u30B9\u30AF\u3092\u81EA\u52D5\u5316\
  \u3057\u305F\u308A\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u30C7\
  \u30FC\u30BF\u3092\u4F9B\u7D66\u3057\u305F\u308A\u3057\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
---

{{< edit_this_page >}}

## 何となぜ？

ClojureでのHTML解析は、プログラムでHTMLドキュメントから情報を抽出することを意味します。プログラマーはこれを行って、ウェブコンテンツにアクセスしたり、動的に操作したり、監視したりすることで、タスクを自動化したり、アプリケーションにデータを供給したりします。

## どのようにして：

ClojureにはHTML解析機能が組み込まれていませんが、Javaライブラリや`enlive`や`hickory`のようなClojureラッパーを活用することができます。以下はその使用方法です：

### Enliveを使用する：

EnliveはHTML解析やウェブスクレイピングに人気の選択肢です。まず、プロジェクトの依存関係に追加してください：

```clojure
[net.cgrand/enlive "1.1.6"]
```

次に、HTMLを解析してナビゲートするには、以下のようにします：

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

このスニペットはHTMLページを取得し、クラス`some-class`を持つ全ての`<div>`要素を選択します。

出力は次のようになるかもしれません：

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Here's some content."]})
```

### Hickoryを使用する：

Hickoryは、Clojureで扱いやすい形式にHTMLを解析する方法を提供します。Hickoryをプロジェクトの依存関係に追加してください：

```clojure
[hickory "0.7.1"]
```

こちらは簡単な例です：

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Hickory形式にHTMLを解析する
(let [doc (hickory/parse "<html><body><div id='main'>Hello, world!</div></body></html>")]
  ;; id'main'のdivを選択する
  (select/select (select/id "main") doc))
```

このコードはシンプルなHTML文字列を解析し、IDが`main`の`div`をCSSセレクタで見つけます。

サンプル出力：

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hello, world!"]}]
```

`enlive`と`hickory`はどちらもClojureでのHTML解析に堅牢なソリューションを提供しており、`enlive`はテンプレートに、`hickory`はデータ変換により重点を置いています。
