---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りとは、プログラムに引数やパラメータを渡す方法です。プログラマーがそれを行う理由は、プログラムの動的な振る舞いを制御するためです。

## どうやって行うか:

Clojureでコマンドライン引数を読み取る最も簡単な方法は、「clojure.core/*command-line-args*」変数を使用することです。

```Clojure
(defn -main [& args]
  (println "コマンドライン引数:" args))

$ lein run arg1 arg2 arg3
コマンドライン引数: [arg1 arg2 arg3]
```

この例では、-main関数はコマンドライン引数を受け取り、それを印刷します。

## 深めの情報

コマンドライン引数の読み取りは、UNIXの初期から存在しています。それはプログラムが自分自身を構成するための基礎的な方法です。

他のClojureのライブラリー、例えば`tools.cli`もコマンドライン引数の読み取りをサポートしています。これらは、より高度なパースオプションやフォーマッティング機能を提供します。

具体的な実装については、基本的にClojureプログラムはJavaプログラムであり、Javaのmainメソッドからコマンドライン引数を取得します。したがって、これらの引数は「clojure.core/*command-line-args*」変数に直接マッピングされます。

## 参考資料

- Clojure公式ドキュメンテーション：https://clojure.org/guides/repl/command_line_args
- Tools.cliライブラリー：https://github.com/clojure/tools.cli
- コマンドライン引数についての詳細な情報：https://stackoverflow.com/questions/30543191/clojure-standalone-program-command-line-argument-parsing