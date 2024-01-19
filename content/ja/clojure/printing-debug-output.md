---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ？

デバッグ出力の印刷は、プログラムが実行中に何を行っているのかを把握するための手段です。このようにして、プログラマーは問題の追跡と解決が容易になります。

## 実装方法：

Clojureでは、デバッグ出力は通常、`println`や`prn`関数を用いて行います。以下に例を示します：

```Clojure
(defn add-two-numbers [a b]
 (let [result (+ a b)]
   (println "Debug output: a =" a ", b =" b ", result =" result)
   result))
```
この場合の`println`の出力は以下のようになります：

```Clojure
(Debug output: a = 5, b = 3, result = 8)
```

## ディープダイブ:

1. 歴史的な文脈: Clojureは、Lispの系譜を継ぐ言語であり、動的型付けと高度な抽象化により、デバッグ出力の表示が容易になります。

2. 代替手段: `println`と`prn`以外にも、`spit`と`slurp`を使用して、デバッグ情報を外部ファイルに出力することもできます。

3. 実装の詳細: `println`は、「改行付きプリント」を意味するものであり、出力後に自動的に改行します。一方、`prn`は、「可読プリント」を意味し、データを人間が読める形式で出力します。

## 参考資料:

1. Official Clojure Documentation: [https://clojure.org/guides/getting_started](https://clojure.org/guides/getting_started)
2. Clojure Debugging Tools: [https://clojure.org/guides/debugging](https://clojure.org/guides/debugging)

以上がClojureによるデバッグ出力の基本です。より深く理解し、適切に活用する事が大切です。