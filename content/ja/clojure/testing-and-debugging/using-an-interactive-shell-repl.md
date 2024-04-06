---
date: 2024-01-26 04:13:34.856012-07:00
description: "\u65B9\u6CD5: REPL\u306FLisp\u30D5\u30A1\u30DF\u30EA\u30FC\u306E\u5BFE\
  \u8A71\u5F0F\u958B\u767A\u54F2\u5B66\u306E\u9375\u3067\u3042\u308A\u3001\u73FE\u4EE3\
  \u306ELisp\u65B9\u8A00\u3067\u3042\u308BClojure\u306F\u3053\u306E\u30C4\u30FC\u30EB\
  \u3092\u5927\u3044\u306B\u6D3B\u7528\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\
  \u306F1950\u5E74\u4EE3\u5F8C\u534A\u306E\u6700\u521D\u306ELisp\u2026"
lastmod: '2024-04-05T21:53:42.507011-06:00'
model: gpt-4-0125-preview
summary: "REPL\u306FLisp\u30D5\u30A1\u30DF\u30EA\u30FC\u306E\u5BFE\u8A71\u5F0F\u958B\
  \u767A\u54F2\u5B66\u306E\u9375\u3067\u3042\u308A\u3001\u73FE\u4EE3\u306ELisp\u65B9\
  \u8A00\u3067\u3042\u308BClojure\u306F\u3053\u306E\u30C4\u30FC\u30EB\u3092\u5927\u3044\
  \u306B\u6D3B\u7528\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u306F1950\u5E74\
  \u4EE3\u5F8C\u534A\u306E\u6700\u521D\u306ELisp REPL\u306B\u307E\u3067\u3055\u304B\
  \u306E\u307C\u308A\u307E\u3059\u3002\u4ED6\u8A00\u8A9E\u306E\u4EE3\u66FF\u54C1\u306B\
  \u306F\u3001Python\u306E\u30A4\u30F3\u30BF\u30FC\u30D7\u30EA\u30BF\u30FC\u3084Node.js\u306E\
  \u30B3\u30F3\u30BD\u30FC\u30EB\u304C\u3042\u308A\u307E\u3059\u304C\u3001Clojure\u306E\
  REPL\u306F\u4E00\u6D41\u306E\u5730\u4F4D\u3092\u6301\u3061\u3001\u30EF\u30FC\u30AF\
  \u30D5\u30ED\u30FC\u306B\u4E0D\u53EF\u6B20\u3067\u3059."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 方法:
REPLを起動して始めます：

```Clojure
user=> (println "Hello, REPL!")
Hello, REPL!
nil
```

関数を定義して試してみます：
```Clojure
user=> (defn greet [name] (str "Hello, " name "!"))
#'user/greet
user=> (greet "Clojure Programmer")
"Hello, Clojure Programmer!"
```

データ構造で実験してください：
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## ディープダイブ
REPLはLispファミリーの対話式開発哲学の鍵であり、現代のLisp方言であるClojureはこのツールを大いに活用しています。これは1950年代後半の最初のLisp REPLにまでさかのぼります。他言語の代替品には、PythonのインタープリターやNode.jsのコンソールがありますが、ClojureのREPLは一流の地位を持ち、ワークフローに不可欠です。

Clojure REPLセッションは、コマンドライン、IDE（IntelliJのCursiveやEmacsのCIDERなど）やNightcodeのようなブラウザーベースのツールなど、さまざまな環境に統合できます。もっと深い意味で、REPLは開発者がランタイムで言語の構造を操作し、さまざまな変換をまたいで状態を維持できるようにし、探索的プログラミングやより堅牢なコードにしばしばつながります。

`lein repl`や`clj`のようなツールでREPLの機能が輝きます。これらは依存関係管理、さまざまなプラグイン、プロジェクト固有のカスタマイズを可能にし、より生産的で柔軟な開発プロセスにつながります。

## 参照
- 公式ClojureウェブサイトガイドのREPL：https://clojure.org/guides/repl/introduction
- Rich HickeyのREPL駆動開発についての話：https://www.youtube.com/watch?v=Qx0-pViyIDU
- 実用的なClojure：REPLを使用した反復的開発：http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html
