---
title:                "文字列を大文字にする"
html_title:           "Clojure: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を大文字に変換するとは、文の開始や名前など、文字列の一部または全体を大文字にすることを指します。プログラマーは読みやすさを高めるためや、データの統一性を保つためにこれを行います。

## 方法：

Clojureでは、`clojure.string` の `upper-case` 関数を使用して文字列を大文字に変換します。以下のコード例と出力結果を参照してください。

```Clojure
(require '[clojure.string :as str])

(str/upper-case "Hello, world!")
```

このコードを実行すると、次のような出力を得ることができます：

```Clojure
"HELLO, WORLD!"
```

## 深掘り

文字列の大文字化は古くからプログラムの中で使われてきました。これはユーザーインターフェースやデータの正規化に重要で、現在でもその価値が認識されています。同様の効果を持つ他の関数には `capitalize`や `title-case`がありますが、これらは通常、特定の文字位置の大文字化に使用されます。 

`upper-case`関数はシンプルで、内部的にはJavaの `toUpperCase`メソッドを呼び出しています。これはUnicode準拠で、全ての文字を所定の大文字にマッピングすることが保証されています。

## 関連情報

- Clojure.string APIドキュメント: https://clojuredocs.org/clojure.string/upper-case
- Clojureの文字列操作全般について: https://clojurebridge.org/community-docs/docs/clojure/clojure-strings/