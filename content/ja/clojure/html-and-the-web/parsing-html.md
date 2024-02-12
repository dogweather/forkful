---
title:                "HTMLの解析"
aliases: - /ja/clojure/parsing-html.md
date:                  2024-02-03T19:11:48.803294-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
