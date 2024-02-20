---
date: 2024-01-20 17:50:37.726743-07:00
description: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5909\u6570\u306A\u3069\
  \u306E\u30C7\u30FC\u30BF\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u57CB\u3081\u8FBC\
  \u3080\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u52D5\
  \u7684\u306B\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u751F\u6210\u3057\u305F\u308A\u3001\
  \u30C7\u30FC\u30BF\u3092\u308F\u304B\u308A\u3084\u3059\u3044\u5F62\u3067\u8868\u793A\
  \u3059\u308B\u6642\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.812031
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u88DC\u9593\u3068\u306F\u3001\u5909\u6570\u306A\u3069\
  \u306E\u30C7\u30FC\u30BF\u3092\u6587\u5B57\u5217\u306E\u4E2D\u306B\u57CB\u3081\u8FBC\
  \u3080\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\u3001\u52D5\
  \u7684\u306B\u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u751F\u6210\u3057\u305F\u308A\u3001\
  \u30C7\u30FC\u30BF\u3092\u308F\u304B\u308A\u3084\u3059\u3044\u5F62\u3067\u8868\u793A\
  \u3059\u308B\u6642\u306B\u4F7F\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
---

{{< edit_this_page >}}

## What & Why?
文字列補間とは、変数などのデータを文字列の中に埋め込むことです。プログラマは、動的にメッセージを生成したり、データをわかりやすい形で表示する時に使います。

## How to:
Clojureには組み込みの文字列補間機能がないため、`str`関数や`format`関数を使います。

```Clojure
;; strを使う場合
(def name "世界")
(println (str "こんにちは、" name "さん!"))

;; 出力: こんにちは、世界さん!

;; formatを使う場合
(def age 30)
(println (format "私は%d歳です。" age))

;; 出力: 私は30歳です。
```

## Deep Dive
Clojureでは直接的な文字列補間はサポートされていませんが、他の関数を通じて似たようなことができます。歴史的には、ClojureはJavaの仮想マシン（JVM）上で動くため、JavaのStringクラスの機能を利用できます。これが`format`関数の根幹です。

文字列補間の別の方法としては、`clojure.pprint/cl-format`関数があります。この関数はCommon Lispの`format`のClojure版です。また、テンプレートエンジンや外部ライブラリを使うこともできます。例えば、`strint`ライブラリはScala風の文字列補間を提供します。

文字列補間の実装詳細に関心があるなら、それぞれの関数のソースコードを見るのがいいでしょう。たとえば`str`は可変長引数を取り、それらを連結します。`format`は`java.lang.String.format`を呼び出します。

## See Also
- Clojureの`str`関数: https://clojuredocs.org/clojure.core/str
- Clojureの`format`関数: https://clojuredocs.org/clojure.core/format
- `cl-format`関数: https://clojuredocs.org/clojure.pprint/cl-format
- `strint`ライブラリ: https://github.com/dakrone/clj-strint
