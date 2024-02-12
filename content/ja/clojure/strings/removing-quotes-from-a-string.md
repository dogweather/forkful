---
title:                "文字列から引用符を削除する"
aliases:
- ja/clojure/removing-quotes-from-a-string.md
date:                  2024-01-26T03:39:20.479915-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/removing-quotes-from-a-string.md"
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
