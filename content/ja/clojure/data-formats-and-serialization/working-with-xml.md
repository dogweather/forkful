---
aliases:
- /ja/clojure/working-with-xml/
date: 2024-01-26 04:29:12.606482-07:00
description: "XML\u306F\u30DE\u30FC\u30AF\u30A2\u30C3\u30D7\u8A00\u8A9E\u3067\u3001\
  \u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u4EBA\u9593\u3068\u6A5F\u68B0\u306E\u4E21\
  \u65B9\u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u30A8\u30F3\u30B3\u30FC\u30C9\u3059\
  \u308B\u305F\u3081\u306E\u3082\u306E\u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\u69CB\
  \u9020\u5316\u3055\u308C\u305F\u968E\u5C64\u5F62\u5F0F\u3067\u6301\u3061\u904B\u3076\
  \u305F\u3081\u3001Web\u30B5\u30FC\u30D3\u30B9\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\
  \u30EB\u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u304A\u3044\u3066\u6B20\u304B\u305B\
  \u307E\u305B\u3093\u3002"
lastmod: 2024-02-18 23:08:54.626140
model: gpt-4-0125-preview
summary: "XML\u306F\u30DE\u30FC\u30AF\u30A2\u30C3\u30D7\u8A00\u8A9E\u3067\u3001\u30C9\
  \u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u4EBA\u9593\u3068\u6A5F\u68B0\u306E\u4E21\u65B9\
  \u304C\u8AAD\u3081\u308B\u5F62\u5F0F\u3067\u30A8\u30F3\u30B3\u30FC\u30C9\u3059\u308B\
  \u305F\u3081\u306E\u3082\u306E\u3067\u3059\u3002\u30C7\u30FC\u30BF\u3092\u69CB\u9020\
  \u5316\u3055\u308C\u305F\u968E\u5C64\u5F62\u5F0F\u3067\u6301\u3061\u904B\u3076\u305F\
  \u3081\u3001Web\u30B5\u30FC\u30D3\u30B9\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\
  \u3001\u30C7\u30FC\u30BF\u4EA4\u63DB\u306B\u304A\u3044\u3066\u6B20\u304B\u305B\u307E\
  \u305B\u3093\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
XMLはマークアップ言語で、ドキュメントを人間と機械の両方が読める形式でエンコードするためのものです。データを構造化された階層形式で持ち運ぶため、Webサービス、設定ファイル、データ交換において欠かせません。

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
