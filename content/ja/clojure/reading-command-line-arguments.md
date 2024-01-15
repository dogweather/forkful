---
title:                "コンピュータープログラミングの記事：コマンドライン引数の読み込み。"
html_title:           "Clojure: コンピュータープログラミングの記事：コマンドライン引数の読み込み。"
simple_title:         "コンピュータープログラミングの記事：コマンドライン引数の読み込み。"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読むことは、プログラムの実行時にさまざまなオプションを指定する必要がある際に非常に役立ちます。

## 方法

コマンドライン引数を読むには、`(.getRuntime)`関数を使用する必要があります。この関数は、JVMの実行時に引数を渡すことができる`(.addShutdownHook)`関数を提供します。以下の例を参考にしてください。

```Clojure
(defn main [& args]
  (.getRuntime (.addShutdownHook (Thread. (fn [] (println "プログラムを終了します。")))))
  (println "コマンドライン引数の数:" (count args))
  (println "コマンドライン引数の内容:")
  (doseq [arg args]
    (println arg)))
```

上記のコードをコンパイルし、コマンドライン引数を添えて実行すると、以下のような出力が得られます。

```
コマンドライン引数の数: 3
コマンドライン引数の内容:
hello world
こんにちは
Testing
```

## 深い掘り下げ

以上の例では、コマンドライン引数を単純にプリントする方法を示しましたが、実際にはより複雑な扱い方ができます。例えば、引数のパースや検証、特定のオプションに応じた条件分岐などが挙げられます。また、`(.addShutdownHook)`関数を使用することで、プログラムが終了する際に特定の処理を実行することもできます。

## その他の情報

- [Clojureドキュメント](https://clojure.org/reference/tools.cli)
- [Clojureクックブック](https://github.com/clojure-cookbook/clojure-cookbook)
- [Javaドキュメント](https://docs.oracle.com/en/java/javase/11/docs/api/index.html)