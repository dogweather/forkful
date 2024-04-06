---
date: 2024-01-26 01:10:20.783890-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u306E\u95A2\u6570\u306F`defn`\u3067\u5B9A\
  \u7FA9\u3055\u308C\u3001\u305D\u306E\u5F8C\u306B\u540D\u524D\u3001\u30D1\u30E9\u30E1\
  \u30FC\u30BF\u3001\u304A\u3088\u3073\u672C\u4F53\u304C\u7D9A\u304D\u307E\u3059\u3002\
  \u3053\u3061\u3089\u304C\u7C21\u5358\u306A\u4F8B\u3067\u3059\u3002"
lastmod: '2024-04-05T21:53:42.512108-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法：
Clojureの関数は`defn`で定義され、その後に名前、パラメータ、および本体が続きます。こちらが簡単な例です。

```Clojure
(defn greet [name]
  (str "Hello, " name "!"))

(greet "Alex") ; => "Hello, Alex!"
```

今度は長方形の面積を計算したいとしましょう。それを全てひとまとめにするのではなく、2つの関数に分けます。

```Clojure
(defn area [length width]
  (* length width))

(defn print-area [length width]
  (println "The area is:" (area length width)))

(print-area 3 4) ; => The area is: 12
```

## 詳細分析
昔のコーダーたちは、全てのロジックを単一のブロックに叩き込んだものです。それは醜かった。その後、構造化プログラミングが登場し、関数が物になりました。Clojureでは、全ての関数はファーストクラスです—それらを他のどんな値のように扱うことができます。

代替案は？一部の人々はマルチメソッドや高階関数を扱うかもしれませんが、それらは関数のシチューの中のスパイスにすぎません。

一関数の細部に全部あります：それらはClojure内で不変です、副作用の混乱が少なくなるでしょう。彼らは典型的なループの代わりに再帰に大いに依存し、それは言語の関数型パラダイムとよく合っています。

## 参照
- Clojure自身のガイド：https://clojure.org/guides/learn/functions
- 関数型プログラミングの基礎：https://www.braveclojure.com/core-functions-in-depth/
- リッチ・ヒッキーの講演：https://changelog.com/posts/rich-hickeys-greatest-hits - Clojureの哲学に関する洞察のために。
