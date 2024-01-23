---
title:                "コマンドライン引数の読み取り"
date:                  2024-01-20T17:56:09.987550-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
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
