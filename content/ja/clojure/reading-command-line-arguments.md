---
title:    "Clojure: コンピュータープログラミングの記事のタイトル:「コマンドライン引数の読み込み」"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読み取るのか

## どのように

コマンドライン引数を読み取ることは、プログラムにとって非常に便利です。JavaやC++などのよく使われる言語では、コマンドライン引数を扱うための組み込み関数があります。しかし、Clojureではそういうものはありません。そのため、私たちは独自の方法でコマンドライン引数を読み取る必要があります。

まず、```(System/getProperty "sun.java.command")```を使用して、コマンドライン引数がどのように渡されるかを確認します。この方法では、プログラムの実行方法によっては意図しない結果を得ることがあります。そのため、次の例を使用して安定的な方法でコマンドライン引数を読み取ります。

```
(ns command-line-args.core
  (:require [clojure.string :as str]))
```

```
(defn get-args []
  (str/split (get (System/getProperties) "sun.java.command") #" ")) 
```

ここで、```clojure.string```モジュールを使用して、スペースで区切られたコマンドライン引数を分割し、配列にすることができます。これで、次のようにコマンドライン引数を読み取ることができます。

```
(defn -main [& args]
  (println (get-args)))
```

このプログラムを ```lein run arg1 arg2 arg3```というように実行すると、```["arg1" "arg2" "arg3"]```という結果が得られます。

## 詳細

Clojureには、```clojure.main```モジュールに含まれる```command-line-args```という関数もあります。これは、コマンドライン引数を文字列の配列として返すことができます。ただし、これはClojureのスクリプトファイルを実行する場合のみ使用でき、```lein```のようなビルドツールを使用する場合は使用できません。

さらに、Clojureでは```clojure.tools.cli```というライブラリを使用することもできます。これは、前述の方法よりも柔軟性があり、より高度なオプションをサポートしています。

# 参考リンク

- [Clojure - Command Line Arguments](https://www.tutorialspoint.com/clojure/clojure_command_line_args.htm)
- [Manipulating command line arguments in Clojure](https://jayway.github.io/clojure/2012/11/16/manipulating-command-line-arguments-in-clojure.html)
- [Reading command line arguments in Clojure](https://blog.worldofcoding.com/2015/07/30/reading-command-line-arguments-in-clojure/)
- [Clojure Tools Command Line Argument Parsing](https://clojure-liberator.github.io/clojure/liberator/configuration/parsing-command-line.html)

# 参照

- [Clojureでコマンドライン引数を取得する方法](https://qiita.com/amapyon/items/b20ebd04fe2cd96f3a69)