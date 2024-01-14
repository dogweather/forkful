---
title:    "Clojure: テキストファイルを書く"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

# なぜテキストファイルを書く必要があるのか

テキストファイルはコンピューターでデータを保存する最も一般的な方法の一つです。プログラミング言語Clojureを使用することで、容易にテキストファイルを作成し、編集することができます。テキストファイルを使うことで、データの検索や編集が簡単になります。

## 作り方

```
;; テキストファイル「sample.txt」を作成する
(with-open [file (clojure.java.io/writer "sample.txt")]
  (clojure.java.io/write file "こんにちは、世界！"))
```

```
;; ファイルを読み込み、内容を表示する
(def text (slurp "sample.txt"))
(println text)
;; Output: こんにちは、世界！
```

## 深堀り

テキストファイルを作成する際には、ファイルの名前や形式を指定することができます。また、ファイルの中身を編集する際には、Clojureの文字列操作の関数を使用することで、柔軟に内容を変更することができます。

以下のコードは、ファイル名や形式を指定し、また文字列操作の関数を使用してファイルの内容を変更する例です。

```
;; テキストファイル「newfile.txt」を作成し、内容を変更する
(with-open [file (clojure.java.io/writer "newfile.txt" :encoding "UTF-8")]
  (clojure.java.io/write file "Hello, world!"))
(println (str/replace (slurp "newfile.txt") "Hello" "Goodbye"))
;; Output: Goodbye, world!
```

# 関連リンク

- [Clojure公式ドキュメント](https://clojure.org/)
- [Teach Yourself Clojure](https://clojure.org/guides/learn/)
- [Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide)
- [テキストファイルの作成や編集に役立つ関数一覧](https://clojuredocs.org/)