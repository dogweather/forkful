---
date: 2024-01-20 17:56:09.987550-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\
  \u53D6\u308A\u3063\u3066\u4F55\uFF1F \u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u8D77\u52D5\
  \u3059\u308B\u3068\u304D\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u5165\u529B\u3057\u305F\
  \u30AA\u30D7\u30B7\u30E7\u30F3\u3084\u30C7\u30FC\u30BF\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u306A\u305C\u4F7F\u7528\u3059\u308B\uFF1F \u4F7F\u3044\u65B9\u3092\
  \u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3057\u305F\u3044\u304B\u3089\u3002\u30B3\u30DE\
  \u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306B\u6307\u793A\u3092\u4F1D\u3048\u308B\u3053\u3068\u304C\u3067\u304D\u308B\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:15.208594-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\
  \u53D6\u308A\u3063\u3066\u4F55\uFF1F \u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u8D77\u52D5\
  \u3059\u308B\u3068\u304D\u3001\u30E6\u30FC\u30B6\u30FC\u304C\u5165\u529B\u3057\u305F\
  \u30AA\u30D7\u30B7\u30E7\u30F3\u3084\u30C7\u30FC\u30BF\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u306A\u305C\u4F7F\u7528\u3059\u308B\uFF1F \u4F7F\u3044\u65B9\u3092\
  \u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\u3057\u305F\u3044\u304B\u3089\u3002\u30B3\u30DE\
  \u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3067\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\
  \u306B\u6307\u793A\u3092\u4F1D\u3048\u308B\u3053\u3068\u304C\u3067\u304D\u308B\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数の読み取りって何？ プログラムが起動するとき、ユーザーが入力したオプションやデータ。プログラマーはなぜ使用する？ 使い方をカスタマイズしたいから。コマンドライン引数で、プログラムに指示を伝えることができる。

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
