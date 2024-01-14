---
title:                "Clojure: コンピュータ・プログラミングの記事「コマンドライン引数の読み込み」"
simple_title:         "コンピュータ・プログラミングの記事「コマンドライン引数の読み込み」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読み取ることは、プログラムをより動的にし、より多くのユーザーのニーズに応えることを可能にします。

## 使い方

コマンドライン引数を読み取るには、```clojure
(System/getProperty "sun.java.command")
``` のように、Javaのプロパティを使用することができます。また、```clojure
(System/getProperty "clojure.args")
``` でも同じ結果を取得できます。

例えば、以下のコードを実行すると：

```clojure
(defn greet [name]
  (println (str "Hello " name "!")))

(defn -main []
  (let [args (System/getProperty "sun.java.command")]
    (greet (nth args 0))))
```

コマンドラインから```java -jar example.jar "John"``` というコマンドを実行すると、"Hello John!"という出力が得られます。このように、コマンドライン引数を受け取って、それを関数に渡すことで、より柔軟なプログラムを作ることができます。

## ディープダイブ

Javaのプロパティを使用してコマンドライン引数を読み取る方法は、オペレーティングシステムやツールによって異なる場合があります。そのため、より複雑なコマンドライン引数の取得を行う場合は、[tools.cli](https://github.com/clojure/tools.cli)や[clj-opts](https://github.com/gliderlabs/clj-opts)などのライブラリを使用することをおすすめします。

## その他

[Javaのpropertiesを使用する方法](https://docs.oracle.com/javase/tutorial/essential/environment/sysprop.html)<br>
[tools.cliのドキュメント](https://github.com/clojure/tools.cli/wiki/Creating-a-command-line)