---
title:    "Clojure: ディレクトリが存在するかどうかをチェックする"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# なぜ
ディレクトリが存在するかどうかを確認することの重要性は、プログラミングの世界ではよく知られています。ディレクトリが存在するかどうかをチェックすることで、プログラムが必要なリソースを正しく見つけることができます。また、ディレクトリが存在しない場合は、エラーを避けることができます。

# 方法
```Clojure
;; 必要な名前空間をインポートする
(ns sample.core
  (:require [clojure.java.io :as io]))

;; ディレクトリのパスを指定する
(def dir-path "/Users/username/Documents")

;; ディレクトリが存在するかどうかをチェックする
(if (io/file dir-path)
  (println "ディレクトリが存在します")
  (println "ディレクトリは存在しません"))
```

上記のように、`clojure.java.io`ライブラリを使用してディレクトリのパスを指定し、`io/file`関数を用いてディレクトリが存在するかどうかをチェックすることができます。`if`文を使って存在の有無に応じて適切なメッセージを出力します。

## スクリーンショット
```
ディレクトリは存在しません
```

# 深堀り
`io/file`関数は、ファイルが存在するかどうかをチェックするためにも使用することができます。また、ディレクトリやファイルの作成や削除など、様々なファイル操作にも応用することができます。さらに、`io/file`関数を使用するために、Javaの標準ライブラリである`java.io.File`も使用可能です。開発者は、自分のプロジェクトに合った方法を選択することができます。

# もっと詳しく知る
- [Official Clojure documentation on io/file](https://clojuredocs.org/clojure.java.io/file)
- [Clojure Cookbook: Checking if a Directory Exists](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02_file_system/2-03_checking-if-a-directory-exists.asciidoc#checking-if-a-directory-exists)
- [Java API documentation for java.io.File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)

# 関連記事
- [Learning Clojure: File Operations](https://www.brainatoms.com/clojure-file-operations/)
- [初めてのClojure: ファイル操作](https://www.brainatoms.com/clojure-file-operations-japanese/)