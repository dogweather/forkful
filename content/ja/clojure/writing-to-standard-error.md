---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
標準エラーとは、プログラムがエラーメッセージや診断情報を出力するストリームのことです。プログラマはこれを使って、エラー情報を標準出力（通常の出力）とは別に扱い、ログ収集やデバッグが容易になるようにします。

## How to:
Clojureで標準エラーに書き込むには、`*err*`を使います。例を見てみましょう。

```Clojure
(spit *err* "エラーが発生しました！\n")
```

出力は標準エラーに直接表示されます。このコードをREPLやスクリプトで実行すると、コンソールにエラーメッセージが出力されます。

## Deep Dive
標準出力と標準エラーはUNIX系のシステムで伝統的に使われてきました。標準出力はファイルディスクリプタ1に、標準エラーは2に割り当てられています。標準エラーへの書き込みは、標準出力とは異なるチャネルを使用するため、出力のリダイレクションやパイプ処理を行っても混ざることがありません。Clojureでは、Javaの`System/err`に対応する`*err*`を通して簡単にアクセスできます。

## See Also
- [Clojureの公式ドキュメンテーション](https://clojure.org/)
- [JavaのPrintStreamクラス](https://docs.oracle.com/javase/7/docs/api/java/io/PrintStream.html)