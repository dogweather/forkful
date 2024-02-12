---
title:                "XMLの扱い方"
aliases:
- /ja/clojure/working-with-xml/
date:                  2024-01-26T04:29:12.606482-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/working-with-xml.md"
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
