---
title:                "正規表現の使用"
html_title:           "Bash: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何となぜ？
正規表現は文字列から特定のパターンを検索、置換したり一致するものを見つけたりする技法です。これがあると、データ検証、スクレイピング、パターンマッチングなどを効率的に行うことができます。

## 使い方:
次のコード例では、特定の文字列("Clojure")がテキスト内に存在するかどうかを確認します。

```Clojure
(defn contains-clojure? [text]
  (re-find #"Clojure" text))

(println (contains-clojure? "I love Clojure programming")) ; => "Clojure"
```

また、次のコード例では、数字を全て抽出します。

```Clojure
(defn extract-numbers [text]
  (re-seq #"\d+" text))

(println (extract-numbers "I have 2 apples and 3 oranges.")) ; => ("2" "3")
```

## ディープダイブ:
正規表現は1950年代に開発され、その後多くのプログラミング言語で採用されてきました。Clojureでも`java.util.regex`パッケージを用いています。

しかし、正規表現を書くのは複雑であるため、必要に応じてORMや専用の検索エンジンなどの代替手段を利用することもあります。

正規表現の動作は、入力テキストの長さに対して時間的に線形となることが多いですが、特定のパターンでは非常に時間がかかることもあります。

## 参考情報:
- Clojure公式ドキュメンテーションの正規表現セクション: [Here](https://clojure.org/reference/other_functions#regex)
- 正規表現の詳細: [Regular-Expressions.info](https://www.regular-expressions.info/)
- Clojureでの正規表現使用例: [Prismatic's Schema](https://github.com/plumatic/schema)