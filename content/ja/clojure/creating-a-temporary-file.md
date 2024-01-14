---
title:    "Clojure: 一時ファイルの作成"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成することに参加する理由は多々あります。例えば、実験的なコードをテストする際や、一時的なデータを処理する際に便利です。

## 作り方

Clojureで一時ファイルを作成するには、`with-open`マクロを使用します。以下は、一時ファイルを作成し、文字列を書き込むサンプルコードです。

```Clojure
(with-open [temp-file (java.io.File/createTempFile "temp" ".txt")]
    (with-open [writer (clojure.java.io/writer temp-file)]
        (.write writer "Hello, world!")))
```

上記のコードを実行すると、カレントディレクトリに`temp.txt`というファイルが作成され、その中に`Hello, world!`という文字列が書き込まれます。

## 深堀り

一時ファイルを作成するたびに、新しいファイルが作成されます。しかし、一時ファイルを使用し終えた後は、手動で削除する必要があります。また、ファイルのパスが必要な場合は、`getAbsolutePath`メソッドを使用することができます。

## その他の参考リンク

- [Clojureドキュメント](https://clojure.org/guides/learn/syntax#_temporary_files)
- [Javaの一時ファイルの作成方法](https://docs.oracle.com/javase/tutorial/essential/io/file.html#temp-file)