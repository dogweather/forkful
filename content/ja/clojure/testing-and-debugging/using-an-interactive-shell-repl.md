---
date: 2024-01-26 04:13:34.856012-07:00
description: "REPL\u3001\u307E\u305F\u306FRead-Eval-Print Loop\u306F\u3001Clojure\u30B3\
  \u30FC\u30C9\u3092\u4E00\u7247\u305A\u3064\u52D5\u7684\u306B\u30C6\u30B9\u30C8\u3059\
  \u308B\u305F\u3081\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\
  \u3059\u3002\u30B3\u30FC\u30C0\u30FC\u306F\u3001\u30B3\u30F3\u30D1\u30A4\u30EB\u3084\
  \u5B8C\u5168\u306A\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u74B0\u5883\u306E\u8A2D\u5B9A\
  \u306E\u30AA\u30FC\u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u3001\u5373\u6642\
  \u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3001\u53CD\u5FA9\u7684\u306A\u958B\u767A\
  \u3001\u8FC5\u901F\u306A\u5B9F\u9A13\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:39.712425-07:00'
model: gpt-4-0125-preview
summary: "REPL\u3001\u307E\u305F\u306FRead-Eval-Print Loop\u306F\u3001Clojure\u30B3\
  \u30FC\u30C9\u3092\u4E00\u7247\u305A\u3064\u52D5\u7684\u306B\u30C6\u30B9\u30C8\u3059\
  \u308B\u305F\u3081\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\
  \u3059\u3002\u30B3\u30FC\u30C0\u30FC\u306F\u3001\u30B3\u30F3\u30D1\u30A4\u30EB\u3084\
  \u5B8C\u5168\u306A\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u74B0\u5883\u306E\u8A2D\u5B9A\
  \u306E\u30AA\u30FC\u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u3001\u5373\u6642\
  \u30D5\u30A3\u30FC\u30C9\u30D0\u30C3\u30AF\u3001\u53CD\u5FA9\u7684\u306A\u958B\u767A\
  \u3001\u8FC5\u901F\u306A\u5B9F\u9A13\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u4F7F\
  \u7528\u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
REPL、またはRead-Eval-Print Loopは、Clojureコードを一片ずつ動的にテストするためのプログラミング環境です。コーダーは、コンパイルや完全なプロジェクト環境の設定のオーバーヘッドなしに、即時フィードバック、反復的な開発、迅速な実験のためにこれを使用します。

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
