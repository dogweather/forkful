---
title:                "コンピュータプログラミングの記事「コマンドライン引数の読み込み」"
html_title:           "Clojure: コンピュータプログラミングの記事「コマンドライン引数の読み込み」"
simple_title:         "コンピュータプログラミングの記事「コマンドライン引数の読み込み」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 読み込みコマンドライン引数

コマンドライン引数の読み込みは、プログラマーが実行中のコマンドラインから情報を受け取ることを意味します。プログラマーは、コマンドライン引数を使用してアプリケーションの動作をカスタマイズしたり、特定の条件を指定したりすることができます。

## 方法：

Clojureでは、``` *command-line-args* ```を使用してコマンドライン引数を読み込むことができます。以下は、コマンドライン引数を読み込んで出力するためのコード例です。

```clojure
(defn -main
  "コマンドライン引数を読み込んで出力する"
  []
  (println "読み込まれたコマンドライン引数: ")
  (println *command-line-args*))

```

コマンドラインでアプリケーションを実行する際に、引数を追加することでコマンドライン引数を渡すことができます。例えば、 ```java -cp myapp.jar myapp.core arg1 arg2```のように実行すると、コマンドライン引数として``` arg1 ```と``` arg2 ```が渡されます。

## 詳細情報：

Javaが登場する前は、コマンドライン引数を読み込むために、Unixシステムで使用されていたC言語の```argv```メソッドが使用されていました。また、Clojure以外の言語でも、同様の機能を持つ```sys.argv```メソッドが利用されています。

Clojureでは、```command-line-args```の他にも、```command-line-args-count```や```command-line-args-str```のような関数も利用できます。

## 関連リンク：

- [Clojure公式ドキュメント](https://clojuredocs.org/clojure.core/command-line-args)
- [Unix-argvメソッドの詳細](https://man7.org/linux/man-pages/man3/argv.3.html)
- [Python-sys.argvメソッドの詳細](https://docs.python.org/3/library/sys.html)