---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:48.803294-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Clojure\u306B\u306F\
  HTML\u89E3\u6790\u6A5F\u80FD\u304C\u7D44\u307F\u8FBC\u307E\u308C\u3066\u3044\u307E\
  \u305B\u3093\u304C\u3001Java\u30E9\u30A4\u30D6\u30E9\u30EA\u3084`enlive`\u3084`hickory`\u306E\
  \u3088\u3046\u306AClojure\u30E9\u30C3\u30D1\u30FC\u3092\u6D3B\u7528\u3059\u308B\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u4F7F\u7528\
  \u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.502099-06:00'
model: gpt-4-0125-preview
summary: ''
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

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
