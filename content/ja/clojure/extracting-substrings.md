---
title:                "Clojure: 「部分文字列を抽出する」"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

今回は、Clojureを使った部分文字列の抽出方法を紹介します。部分文字列の抽出は、文字列から任意の部分を取り出して処理する必要がある場合に非常に便利です。例えば、あるフルネームが与えられた場合、苗字だけを取り出したいときなどに活用することができます。

## 方法

部分文字列を抽出するには、`subs`という関数を使用します。以下のように、文字列と抽出したい開始位置と終了位置を指定することで、部分文字列を抽出することができます。

```Clojure
(def fullname "山田太郎")
(str/subs fullname 0 2)  ;; 結果: "山田"
```

また、正規表現を使ってより柔軟な部分文字列の抽出も可能です。正規表現を用いる場合は、`re-find`関数を使用します。例えば、以下のように苗字の部分を正規表現で指定することで、苗字を抽出することができます。

```Clojure
(def fullname "山田太郎")
(def regex #"[一-龠]+")
(str/re-find regex fullname)  ;; 結果: "山田"
```

## ディープダイブ

部分文字列の抽出に関しては、文字列や正規表現だけでなく、Clojureのシーケンスやコレクションに対しても使用することができます。これにより、より複雑なデータに対しても効率的に部分文字列を抽出することができます。

例えば、以下のように文字列のリストを定義し、`map`関数を使って全ての要素に対して苗字を抽出することができます。

```Clojure
(def fullnames ["山田太郎", "鈴木花子", "佐藤健太"])
(def regex #"[一-龠]+")
(map #(str/re-find regex %) fullnames)  ;; 結果: ("山田", "鈴木", "佐藤")
```

## 参考文献

- [Clojure Docs - subs](https://clojuredocs.org/clojure.core/subs)
- [Clojure Docs - re-find](https://clojuredocs.org/clojure.core/re-find)

## 参考になるリンク

- [Clojure で文字列操作をする方法](https://qiita.com/delicious-nimifuse/items/43e61b2e64574f2b1586)
- [Clojureで正規表現を使う方法](https://qiita.com/sbre/items/8052ddc443e8d9d16442)