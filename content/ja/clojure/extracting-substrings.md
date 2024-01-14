---
title:    "Clojure: 部分文字列の抽出"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# なぜ？

サブストリング（部分文字列）を抽出することの利点はいくつかあります。例えば、テキストから特定のキーワードを含む文を抽出したり、文字列の特定の部分を取り出したりすることができます。また、文字列の操作やデータ処理においても便利です。Clojureでは、文字列の抽出に便利な関数が用意されていますので、ぜひ活用してみてください。

## 方法

サブストリングの抽出には、`subs`や`subseq`などの関数が使えます。これらの関数は、取り出したい部分のインデックスを指定するだけで、簡単にサブストリングを取り出すことができます。

```Clojure
(def sample-str "これはサンプル文字列です。")
(subs sample-str 3 8)
;=> "はサンプ"
(subseq sample-str 0 5)
;=> "これはサン"
```

また、正規表現を使うことでより柔軟にサブストリングを抽出することもできます。

```Clojure
(require '[clojure.string :as str])
(str/replace "Hello World" #"(\w+)\s(\w+)" "$2 $1")
;=> "World Hello"
```

## 深堀り

サブストリングを抽出する際、注意すべき点がいくつかあります。例えば、文字列の末尾に近いインデックスを指定する際には、文字列の長さを超えないように気をつける必要があります。また、正規表現を使う場合には、パターンに対応する文字列が見つからない場合にはエラーが発生する可能性があるので、エラーハンドリングも重要です。

# 参考

- [ClojureDocs - subs](https://clojuredocs.org/clojure.core/subs)
- [ClojureDocs - subseq](https://clojuredocs.org/clojure.core/subseq)
- [ClojureDocs - Regular Expression](https://clojuredocs.org/clojure.string/split-lines)"

# 参考文献

- [ClojureDocs - subs](https://clojuredocs.org/clojure.core/subs)
- [ClojureDocs - subseq](https://clojuredocs.org/clojure.core/subseq)
- [ClojureDocs - 正規表現](https://clojuredocs.org/clojure.string/split-lines)