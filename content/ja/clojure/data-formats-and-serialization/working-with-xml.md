---
date: 2024-01-26 04:29:12.606482-07:00
description: "\u65B9\u6CD5: Clojure\u306F`clojure.data.xml`\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092XML\u306E\u89E3\u6790\u3068\u51FA\u529B\u306B\u63D0\u4F9B\u3057\u307E\
  \u3059\u3002\u307E\u305A\u3001\u3044\u304F\u3064\u304B\u306EXML\u3092\u89E3\u6790\
  \u3057\u3066\u307F\u307E\u3057\u3087\u3046\uFF1A."
lastmod: '2024-03-13T22:44:41.588792-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u306F`clojure.data.xml`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092XML\u306E\
  \u89E3\u6790\u3068\u51FA\u529B\u306B\u63D0\u4F9B\u3057\u307E\u3059\u3002\u307E\u305A\
  \u3001\u3044\u304F\u3064\u304B\u306EXML\u3092\u89E3\u6790\u3057\u3066\u307F\u307E\
  \u3057\u3087\u3046\uFF1A."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 方法:
Clojureは`clojure.data.xml`ライブラリをXMLの解析と出力に提供します。まず、いくつかのXMLを解析してみましょう：

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; XML文字列を解析
  (println parsed))
```
出力：
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Clojure構造体からXMLを出力するには：

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
出力：
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## ディープダイブ
XMLは90年代後半から存在し、SGMLの単純化されたサブセットとしてWebデータ用に開始されました。SOAPやXHTMLなどの技術とともに使用が爆発的に増えましたが、軽量でシンプルであるためJSONからの競合もあります。

ClojureのXMLへのアプローチは、関数型でデータ中心のままで、言語の理念に忠実です。`clojure.data.xml`は選択肢の一つに過ぎず、基本的なニーズには`clojure.xml`を、JavaインタロップにはJAXBやDOM4Jなどの主要なツールを使用できます。

非常に大きなXMLドキュメントを扱う場合、パフォーマンスとメモリのオーバーヘッドが重くなる点に注意してください。ストリーミングパーサーのStAXが役立つかもしれませんが、それにはJava側へ移動する必要があります。

## 参照
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
