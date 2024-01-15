---
title:                "「文字列を小文字に変換する」"
html_title:           "Clojure: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## クロージャーで文字列を小文字に変換する理由

文字列を小文字に変換することは、コンピュータ上の文字列操作で非常に一般的です。例えば、ユーザーが入力した文字列を統一したフォーマットで処理するために、小文字に変換する必要があるかもしれません。または、検索エンジンやソート機能を実装する際に、文字列を小文字に変換してより柔軟な検索やソートができるようにするためです。

## クロージャーで文字列を小文字に変換する方法

変換したい文字列を `lower-case` 関数に渡すだけで簡単に文字列を小文字に変換することができます。以下のコード例を参考にしてください。

```Clojure
(lower-case "Hello, World!")
```

上記のコードを実行すると、出力は以下のようになります。

```Clojure
"hello, world!"
```

## クロージャーで文字列を小文字に変換する深堀り

クロージャーの `lower-case` 関数は、文字列を小文字に変換するだけでなく、国際化や地域毎の文字列操作にも対応しています。また、多言語環境での文字列比較やパターンマッチングにも有用です。詳しくは公式ドキュメントをご覧ください。

## 参考リンク

- `lower-case` 関数の公式ドキュメント: https://clojure.org/reference/strings#clojure.string/lower-case
- 国際化や地域毎の文字列操作に関する詳細: https://github.com/clojure/clojure/blob/master/src/clj/clojure/string.clj#L832-L910
- マルチバイト文字列処理に関する記事 (英語): https://lambdaisland.com/blog/2019-04-11-how-to-compare-strings-in-clojure
- パターンマッチングに関するチュートリアル (英語): https://www.braveclojure.com/functional-programming/#Pattern_Matching

## 関連リンク

- クロージャー公式ドキュメント: https://clojure.org/
- クロージャーについて知りたい方への入門記事: https://qiita.com/t-watanabe/items/efd6e4d033dc7263afed
- クロージャーを使った実践的なプログラミングの例: https://github.com/karad/aroha/blob/master/README.md