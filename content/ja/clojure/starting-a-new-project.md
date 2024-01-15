---
title:                "新しいプロジェクトの開始"
html_title:           "Clojure: 新しいプロジェクトの開始"
simple_title:         "新しいプロジェクトの開始"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ
新しいプロジェクトを開始することの利点は何ですか？
新しいプロジェクトを始めることは、新しい技術やアイデアを学ぶことや、新しい可能性を発見することができるチャンスです。

## 使い方
まず、Clojureのインストール方法を確認しましょう。次に、プロジェクトを作成するためのツールであるLeiningenをインストールします。そして、Clojureのコードを書くためのテキストエディタを選び、プロジェクトフォルダ内で```lein new app my-project```を実行し、プロジェクトを作成します。Clojureでは、関数を宣言するために```defn```を使用し、それを実行するためには関数名の後に必要な引数を入力します。以下の例を参考にしてみてください。

```Clojure
(defn insert-two [num]
  (+ num 2))
(insert-two 3)

;; => 5
```

## ディープダイブ
新しいプロジェクトを始めるためには、まずどのような目的を持っているのかを明確にすることが重要です。それから、必要なライブラリやフレームワークを選び、プロジェクトの構造を決めることができます。また、Clojureでは関数型プログラミングをサポートしているため、関数の戻り値や引数に対して処理を行うことができます。このように、Clojureは柔軟でスケーラブルなプログラミング言語です。

## 参考リンク
- [Clojure公式サイト](https://clojure.org/)
- [Leiningenの使い方](https://github.com/technomancy/leiningen/blob/master/doc/TUTORIAL.md)
- [プログラミング言語Clojureの基本](https://qiita.com/hkusu/items/ef457b2fb1c29aba7c27)