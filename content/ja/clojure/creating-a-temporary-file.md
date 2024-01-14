---
title:    "Clojure: 一時ファイルの作成"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成する理由はたくさんあります。例えば、大きなデータを処理する中間ファイルを作成したり、一時的なバックアップファイルを保存することができます。また、一時ファイルはアプリケーションが終了した後自動的に削除されるため、メモリを節約することができます。

## 作り方

一時ファイルを作成するには、Clojureのライブラリであるclojure.java.ioを使用します。まず、適切なパッケージをインポートします。

```Clojure
(ns my-app.core
  (:require [clojure.java.io :as io]))
```

次に、`with-open`マクロを使用して一時ファイルを作成します。以下の例では、一時ファイルを作成し、そのパスを出力しています。

```Clojure
(with-open [file (io/file "tempfile.txt")]
  (println (.getPath file)))
```

出力結果は以下のようになります。

```
/tmp/tempfile.txt
```

また、作成された一時ファイルにデータを書き込むこともできます。以下の例では、一時ファイルに"Hello World"という文字列を書き込んでいます。

```Clojure
(with-open [file (io/file "tempfile.txt")]
  (io/write file "Hello World"))
```

一時ファイルを削除するには、`delete-file`関数を使用します。

```Clojure
(io/delete-file "tempfile.txt")
```

## 深堀り

一時ファイルを作成する際には、一時ファイルの作成先を指定することもできます。例えば、`with-open`マクロの第一引数にディレクトリパスを指定することで、指定したディレクトリに一時ファイルを作成することができます。

また、一時ファイルを作成する際には、一意のファイル名を指定することも重要です。そのために、Clojureでは`uuid`関数を使用して一意のファイル名を生成することができます。

```Clojure
(with-open [file (io/file "/tmp/" (str (uuid) ".txt"))]
  (io/write file "Hello World"))
```

## 他の参考文献

- [clojure.java.ioライブラリのドキュメンテーション](https://clojure.github.io/java.io/)
- [with-openマクロの詳細](https://clojuredocs.org/clojure.core/with-open)