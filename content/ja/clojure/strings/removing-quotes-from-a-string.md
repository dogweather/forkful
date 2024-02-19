---
aliases:
- /ja/clojure/removing-quotes-from-a-string/
date: 2024-01-26 03:39:20.479915-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\u3080\u3046\u3063\u3068\
  \u3046\u3057\u3044\u4E8C\u91CD\u5F15\u7528\u7B26\u307E\u305F\u306F\u5358\u4E00\u5F15\
  \u7528\u7B26\u306E\u6587\u5B57\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u610F\
  \u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\
  \u3092\u884C\u3063\u3066\u30C7\u30FC\u30BF\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\
  \u30D7\u3057\u3001\u7D71\u4E00\u6027\u3092\u78BA\u4FDD\u3057\u305F\u308A\u3001\u5F15\
  \u7528\u7B26\u304C\u671B\u307E\u3057\u304F\u306A\u3044\u304B\u30A8\u30E9\u30FC\u3092\
  \u5F15\u304D\u8D77\u3053\u3059\u53EF\u80FD\u6027\u304C\u3042\u308B\u51E6\u7406\u306E\
  \u305F\u3081\u306B\u6587\u5B57\u5217\u3092\u6E96\u5099\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.592501
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u56F2\u3080\u3046\u3063\u3068\
  \u3046\u3057\u3044\u4E8C\u91CD\u5F15\u7528\u7B26\u307E\u305F\u306F\u5358\u4E00\u5F15\
  \u7528\u7B26\u306E\u6587\u5B57\u3092\u53D6\u308A\u9664\u304F\u3053\u3068\u3092\u610F\
  \u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\
  \u3092\u884C\u3063\u3066\u30C7\u30FC\u30BF\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\
  \u30D7\u3057\u3001\u7D71\u4E00\u6027\u3092\u78BA\u4FDD\u3057\u305F\u308A\u3001\u5F15\
  \u7528\u7B26\u304C\u671B\u307E\u3057\u304F\u306A\u3044\u304B\u30A8\u30E9\u30FC\u3092\
  \u5F15\u304D\u8D77\u3053\u3059\u53EF\u80FD\u6027\u304C\u3042\u308B\u51E6\u7406\u306E\
  \u305F\u3081\u306B\u6587\u5B57\u5217\u3092\u6E96\u5099\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく？
文字列から引用符を取り除くとは、テキストを囲むうっとうしい二重引用符または単一引用符の文字を取り除くことを意味します。プログラマーはこれを行ってデータをクリーンアップし、統一性を確保したり、引用符が望ましくないかエラーを引き起こす可能性がある処理のために文字列を準備します。

## 方法：
Clojureでは、文字列は不変であるため、「引用符を除去する」と話す時、実際には引用符なしの新しい文字列を作成していることになります。ここに `clojure.string/replace` を使用してざっくりとやり方を示します：

```clojure
(require '[clojure.string :as str])

; 二重引用符を取り除きましょう
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; そして単一引用符を排除します
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; サンプル使用法：
(remove-double-quotes "\"Hello, World!\"") ; => "Hello, World!"
(remove-single-quotes "'Hello, World!'")   ; => "Hello, World!"
```
一度に単一引用符と二重引用符の両方を処理したいですか？これを見てください：

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; サンプル使用法：
(remove-quotes "\"Hello, 'Clojure' World!\"") ; => "Hello, Clojure World!"
```

## 深堀り
昔、データが子供の寝室のように散らかっていた時代には、文字列内の引用符がテキストを示すための標準でした。しかし、コンピュータサイエンスが進化するにつれて、引用符はただのテキスト区切り以上のものとなり、プログラミング言語では構文上の役割を担うようになりました。

Lispの遺産を持つClojureは、他の言語がするように引用符を同じ方法で使用していません。確かに文字列を示すために使用されていますが、リテラルを作成する際に特別な役割も担っています。それにもかかわらず、文字列から引用符を取り除くことは時代を超えたタスクのままです。

なぜ単に文字列の端を切り取らないのでしょうか？それは、引用符がいつも文字列の始まりと終わりを過剰に愛情を持って包むかのように仮定しているからです。現実のデータはもっと雑然としています。そこで正規表現（regex）が登場し、どこに隠れていてもその引用符をターゲットにすることができます。

代替手段は？確かに、`subs`, `trim`, `triml`, `trimr`、さらには自慢したい場合はトランスデューサーを使って凝ったことをすることもできます。しかし、正規表現を使った`replace`は、ナイフ戦にライトセーバーを持ち込むようなものです—それは直接問題の核心に切り込みます。

## 参照
もっとClojureの文字列操作について頭を悩ませたい場合は、これらの手がかりが役立つかもしれません：
- `clojure.string/replace`についてのClojureDocs：https://clojuredocs.org/clojure.string/replace
- Clojureでの正規表現：https://clojure.org/guides/learn/syntax#_regex
- 文字列処理のためのJavaインタロップ（Clojureは結局のところJVM上で動作します）：https://clojure.org/reference/java_interop#_working_with_strings

引用符の除去で止まることはありません。Clojure-landには発見されるべき文字列の魔法の全世界があります。
