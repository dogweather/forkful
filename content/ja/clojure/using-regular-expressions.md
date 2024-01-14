---
title:                "Clojure: 正規表現の使用"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

使用正規表現の理由:

正規表現は、文字列を検索や置換したりといったテキスト処理を行うために非常に便利なツールです。また、パターンを用いて文書を高度にマッチングすることも可能です。Clojureで正規表現を使用することで、テキスト処理の速度が向上し、コードをよりクリーンに保つことができます。

方法:

正規表現は、さまざまな方法で使用できます。まずは、```regex```関数を使用してパターンを定義し、```re-find```や```re-groups```などの関数を使用してテキスト処理を行います。以下は、```re-find```を使用して文字列内の数値を抽出する例です。

```Clojure
(defn find-numbers [text]
  (re-find #"\d+" text))

(find-numbers "今日の気温は22度です。")
;; => "22"
```

さらに、正規表現を用いて文字列を置換することもできます。例えば、以下のように```re-seq```を使用して、文字列内の全ての空白を除去することができます。

```Clojure
(defn remove-spaces [text]
  (apply str (re-seq #"\S" text)))

(remove-spaces "Hello World")
;; => "HelloWorld"
```

深堀り:

正規表現をより理解するために、以下のリンクを参考にすることをお勧めします。

見る価値あり:

- [Clojure 正規表現チュートリアル](https://www.tutorialspoint.com/clojure/clojure_regular_expressions.htm)
- [正規表現クイックシート](https://www.rexegg.com/regex-quickstart.html)
- [Clojure 正規表現リファレンス](https://clojure.github.io/clojure/clojure.string-api.html#clojure.string/re-find)