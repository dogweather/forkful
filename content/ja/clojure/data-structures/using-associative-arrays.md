---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:25.371851-07:00
description: "\u65B9\u6CD5\uFF1A Clojure\u3067\u9023\u60F3\u914D\u5217\uFF08\u30CF\
  \u30C3\u30B7\u30E5\u30DE\u30C3\u30D7\uFF09\u3092\u4F5C\u6210\u3057\u3066\u64CD\u4F5C\
  \u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\
  \u307E\u3057\u3087\u3046\u3002 \u30CF\u30C3\u30B7\u30E5\u30DE\u30C3\u30D7\u3092\u4F5C\
  \u6210\u3059\u308B\u306B\u306F\uFF1A."
lastmod: '2024-04-05T21:53:42.496409-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法：
Clojureで連想配列（ハッシュマップ）を作成して操作するのは簡単です。例を見てみましょう。

ハッシュマップを作成するには：

```clojure
(def my-map {:name "Alex" :age 30})
```

キーを指定して値を取得できます：

```clojure
(get my-map :name)
;; "Alex"
```
または、もっと慣用的にキーを関数として使うこともできます：

```clojure
(:name my-map)
;; "Alex"
```

エントリの追加や更新は簡単です：

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

キーを削除するには`dissoc`を使用します：

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

マップを反復処理するには：

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

条件付きアクセスのために、`find`はキーが存在する場合にキーと値のペアを返します：

```clojure
(find my-map :age)
;; [:age 30]
```

## 深堀り
Clojureの連想配列（通常、ハッシュマップとしても参照されます）は、キーと値に基づくデータを管理するために非常に汎用性が高く効率的です。それらはClojureの豊富なコレクションライブラリの一部であり、言語の不変性と関数型プログラミングの哲学に深く根ざしています。要素にアクセスするためにO(n)の時間複雑性を必要とする配列やリストとは異なり、ハッシュマップはアクセスにほぼ定数の時間複雑性を提供し、ルックアップ操作において非常に効率的です。

Clojureにおけるベクターがインデックスによるアクセスを通じて類似の目的を果たすと主張することもできますが、非連続でラベル付けされたデータを扱う場合に、キーが任意のインデックスではなく意味のある記述子を提供するとき、ハッシュマップが優れています。

Clojure（及びそのLispの遺産）に特有のものとして、連想配列は第一級の市民であり、特別な構文やアクセス方法を必要とすることなく、直接操作したり、関数に渡したりすることができます。この設計決定は、Clojureのシンプルさとパワーに対する重点を強調しています。

ハッシュマップは非常に便利ですが、非常に大きなデータセットを扱う場合やキーが非常に動的な（追加と削除を常に行う）場面では、代替のデータ構造やデータベースがより良いパフォーマンスと柔軟性を提供するかもしれません。しかし、Clojureアプリケーションの典型的な使用例の範囲内で、連想配列はデータ管理の堅牢で効率的な手段を提供します。
