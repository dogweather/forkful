---
date: 2024-01-26 03:39:20.479915-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u3067\u306F\u3001\u6587\u5B57\u5217\u306F\
  \u4E0D\u5909\u3067\u3042\u308B\u305F\u3081\u3001\u300C\u5F15\u7528\u7B26\u3092\u9664\
  \u53BB\u3059\u308B\u300D\u3068\u8A71\u3059\u6642\u3001\u5B9F\u969B\u306B\u306F\u5F15\
  \u7528\u7B26\u306A\u3057\u306E\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u4F5C\u6210\
  \u3057\u3066\u3044\u308B\u3053\u3068\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u3053\
  \u306B `clojure.string/replace` \u3092\u4F7F\u7528\u3057\u3066\u3056\u3063\u304F\
  \u308A\u3068\u3084\u308A\u65B9\u3092\u793A\u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.541241-06:00'
model: gpt-4-0125-preview
summary: "Clojure\u3067\u306F\u3001\u6587\u5B57\u5217\u306F\u4E0D\u5909\u3067\u3042\
  \u308B\u305F\u3081\u3001\u300C\u5F15\u7528\u7B26\u3092\u9664\u53BB\u3059\u308B\u300D\
  \u3068\u8A71\u3059\u6642\u3001\u5B9F\u969B\u306B\u306F\u5F15\u7528\u7B26\u306A\u3057\
  \u306E\u65B0\u3057\u3044\u6587\u5B57\u5217\u3092\u4F5C\u6210\u3057\u3066\u3044\u308B\
  \u3053\u3068\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u3053\u306B `clojure.string/replace`\
  \ \u3092\u4F7F\u7528\u3057\u3066\u3056\u3063\u304F\u308A\u3068\u3084\u308A\u65B9\
  \u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

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
