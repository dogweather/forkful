---
date: 2024-01-20 17:56:09.987550-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:53.990178-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306F\
  \u3001Unix\u306E\u6642\u4EE3\u304B\u3089\u3042\u308B\u3002\u74B0\u5883\u5909\u6570\
  \u3084\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3082\u30D1\u30E9\u30E1\u30FC\u30BF\u30FC\
  \u3092\u8A2D\u5B9A\u3059\u308B\u65B9\u6CD5\u3060\u304C\u3001\u3088\u308A\u52D5\u7684\
  \u306B\u6307\u793A\u3092\u4E0E\u3048\u305F\u3044\u6642\u306B\u5F15\u6570\u304C\u9078\
  \u3070\u308C\u308B\u3002Clojure\u3067\u306F\u3001`-main`\u95A2\u6570\u3092\u5B9A\
  \u7FA9\u3059\u308B\u3068\u3001\u53EF\u5909\u500B\u5F15\u6570\u3068\u3057\u3066\u30B3\
  \u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u53D7\u3051\u53D6\u308B\u3002\
  \u5F15\u6570\u306F\u6587\u5B57\u5217\u306E\u30D9\u30AF\u30C8\u30EB\u3068\u3057\u3066\
  \u51E6\u7406\u304C\u884C\u308F\u308C\u308B."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

## How to: (方法)
```Clojure
;; “args” refers to a vector of arguments passed from the command line
(defn -main [& args]
  (println "Command line arguments:" args))

;; Run the program in terminal:
;; $ clojure -m your-namespace-here argument1 argument2

;; Sample output:
;; Command line arguments: [argument1 argument2]
```

## Deep Dive (深い情報)
コマンドライン引数は、Unixの時代からある。環境変数や設定ファイルもパラメーターを設定する方法だが、より動的に指示を与えたい時に引数が選ばれる。Clojureでは、`-main`関数を定義すると、可変個引数としてコマンドライン引数を受け取る。引数は文字列のベクトルとして処理が行われる。

LeiningenやBootなどのビルドツールを使い、コマンドラインからClojureプログラムを実行することが多い。Clojure 1.9から、cljスクリプトとdeps.ednも使われるようになった。

実装については、argsは基本的に`*command-line-args*`グローバル変数で管理されるが、これは避けるべき。その理由は、引数がグローバルに存在すると、関数が外部の状態に依存することになり、関数型プログラミングの原則に反するから。

## See Also (関連情報)
- Clojure公式ドキュメント: [https://clojure.org/](https://clojure.org/)
- コマンドラインアプリのClojureガイド: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)
- Leiningenユーザーガイド: [https://leiningen.org/](https://leiningen.org/)
- Bootビルドツール: [http://boot-clj.com/](http://boot-clj.com/)
- `tools.cli`ライブラリ (引数パース用): [https://github.com/clojure/tools.cli](https://github.com/clojure/tools.cli)
