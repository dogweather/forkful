---
title:                "Clojure: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ
Charater（文字）が特定のパターンに一致するものを削除するのに、人々が参加する理由を1〜2文で説明します。

## 方法
```Clojure
;; 削除前の文字列
(def str "abcd1234efgh5678")

;; パターンに一致する文字を削除する関数
(defn delete-matching [str pattern]
  (clojure.string/replace str pattern ""))

;; パターンに一致する文字を削除
(delete-matching str #"([a-z]+|[0-9]+)")

;; 出力結果：""
```

## 深堀り
文字のパターンに一致するものを削除することは、文字列の処理において非常に便利な方法です。Clojureでは、`clojure.string`ライブラリの`replace`関数を使用して、パターンに一致するものを簡単に削除することができます。

また、パターンには正規表現を使用することができるため、より複雑な文字列の処理にも役立ちます。例えば、特定の文字を置換したり、部分文字列を抽出したりすることができます。

## 参考リンク
- [Clojure.org | string-api](https://clojure.org/api/cheatsheet)
- [ClojureDocs | string](https://clojuredocs.org/clojure.string)
- [正規表現入門](https://qiita.com/jnchito/items/b274319edf3859f8b26f)