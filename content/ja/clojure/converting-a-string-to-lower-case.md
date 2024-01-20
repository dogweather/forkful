---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、基本的に大文字を小文字にする操作です。これは、一致テスト、ソート、または他の比較操作を実行する前の文字列の正準化が必要な際にプログラマーが行います。

## 使い方：

以下のようなコード例とその結果を参考にしてください：

```Clojure
(defn to-lower-case [str]
  (.toLowerCase str))
```

この関数は、指定された文字列をすべて小文字に変換します。以下に例を示します。

```Clojure
(println (to-lower-case "HELLO WORLD")) ;; => hello world
```

## 深堀り：

文字列の小文字化はコンピューティングの歴史の初期から存在していました。最初は機械語やアセンブリ言語レベルで操作が行われていましたが、現在ではほぼすべての高級プログラミング言語で組み込み関数として使用できます。

代替手段としては、特定の状況（例えば、特定のキャラクタセットでの使用）で自分で関数を書くか、または異なる方法で大文字と小文字を無視することなどがあります。

`toLowerCase`メソッドの内部実装により、無視してよい詳細な差異が存在します。在り方としては、特定の状況下での特別な文字の変換（例えば、トルコ語の"ı"のような）をうまく処理します。

## 参考資料：

- Clojure String API - https://clojuredocs.org/clojure.string/lower-case
- UnicodeとCase Mapping - http://www.unicode.org/versions/Unicode7.0.0/ch03.pdf#G33902
- 他のプログラミング言語での文字列の小文字化 - https://rosettacode.org/wiki/String_case