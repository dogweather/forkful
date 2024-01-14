---
title:                "Clojure: 一時ファイルの作成"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由は様々です。例えば、ファイルを一時的に保存する必要がある時や、あるプログラムの出力を保存する時などに使えます。また、一時ファイルはメモリを節約することにも役立ちます。

## 作り方

Clojureでは、`with-open`関数を使うと簡単に一時ファイルを作成することができます。以下のコードを参考にしてください。

```Clojure
(with-open [file (java.io.File/createTempFile "prefix-" ".txt")]
  ; ファイルにデータを書き込む
  (.write file "Hello World!")
  ; ファイルを閉じる
)
```

上記の例では、`with-open`関数を使って一時ファイルを作成し、そのファイルに`write`メソッドでデータを書き込み、最後にファイルを閉じています。こうすることで、一時ファイルが自動的に削除されるので、メモリを節約することができます。

## より詳しく

一時ファイルを作成する際には、`java.io.File`クラスの`createTempFile`メソッドを使用します。このメソッドには2つの引数があります。最初の引数は、一時ファイルのプレフィックスを表し、2つ目の引数は一時ファイルの拡張子を表します。また、一時ファイルはデフォルトではシステムの一時フォルダに作成されますが、`java.io.File`クラスの`createTempFile`メソッドの第3引数を指定することで、ファイルを作成する場所を指定することもできます。

## その他の情報

一時ファイルを作成する際には、メモリの管理やファイルの確保について注意する必要があります。一時ファイルが多すぎるとメモリが不足し、プログラムが正しく動作しない可能性があります。また、ファイルの削除を忘れるとディスク容量を浪費することにもつながります。ですので、一時ファイルを使う際には、適切なタイミングでファイルを閉じて削除するように注意しましょう。

## 関連リンク

- [Java SE ドキュメント - java.io.Fileクラス](https://docs.oracle.com/javase/jp/8/docs/api/java/io/File.html)
- [Clojureホームページ](https://clojure.org/)
- [Clojureドキュメント - with-open関数](https://clojuredocs.org/clojure.core/with-open)