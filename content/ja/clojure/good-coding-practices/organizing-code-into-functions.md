---
date: 2024-01-26 01:10:20.783890-07:00
description: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u307E\u3068\u3081\u308B\u3053\
  \u3068\u306F\u3001\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3059\u308B\
  \u30B3\u30FC\u30C9\u30D6\u30ED\u30C3\u30AF\u3092\u30D1\u30C3\u30B1\u30FC\u30B8\u5316\
  \u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u3053\u308C\u3092\
  \u884C\u3046\u3053\u3068\u3067\u3001\u30B3\u30FC\u30C9\u3092\u30AF\u30EA\u30FC\u30F3\
  \u306B\u3057\u3001\u4FDD\u5B88\u304C\u5BB9\u6613\u306B\u306A\u308A\u3001\u4ED6\u306E\
  \u958B\u767A\u8005\u304C\u8AAD\u307F\u3084\u3059\u304F\u306A\u308A\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.839928
model: gpt-4-1106-preview
summary: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u307E\u3068\u3081\u308B\u3053\
  \u3068\u306F\u3001\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\u3092\u5B9F\u884C\u3059\u308B\
  \u30B3\u30FC\u30C9\u30D6\u30ED\u30C3\u30AF\u3092\u30D1\u30C3\u30B1\u30FC\u30B8\u5316\
  \u3059\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\u3002\u3053\u308C\u3092\
  \u884C\u3046\u3053\u3068\u3067\u3001\u30B3\u30FC\u30C9\u3092\u30AF\u30EA\u30FC\u30F3\
  \u306B\u3057\u3001\u4FDD\u5B88\u304C\u5BB9\u6613\u306B\u306A\u308A\u3001\u4ED6\u306E\
  \u958B\u767A\u8005\u304C\u8AAD\u307F\u3084\u3059\u304F\u306A\u308A\u307E\u3059\u3002"
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく、なぜ？

コードを関数にまとめることは、特定のタスクを実行するコードブロックをパッケージ化することについてです。これを行うことで、コードをクリーンにし、保守が容易になり、他の開発者が読みやすくなります。

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
