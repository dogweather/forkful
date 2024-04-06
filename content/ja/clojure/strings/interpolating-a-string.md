---
date: 2024-01-20 17:50:37.726743-07:00
description: "How to: Clojure\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u6587\u5B57\
  \u5217\u88DC\u9593\u6A5F\u80FD\u304C\u306A\u3044\u305F\u3081\u3001`str`\u95A2\u6570\
  \u3084`format`\u95A2\u6570\u3092\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.488155-06:00'
model: gpt-4-1106-preview
summary: "Clojure\u306B\u306F\u7D44\u307F\u8FBC\u307F\u306E\u6587\u5B57\u5217\u88DC\
  \u9593\u6A5F\u80FD\u304C\u306A\u3044\u305F\u3081\u3001`str`\u95A2\u6570\u3084`format`\u95A2\
  \u6570\u3092\u4F7F\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

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
