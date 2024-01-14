---
title:                "Clojure: 文字列を小文字に変換する"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ？

文字列を小文字に変換する理由は、コーディングをより効率的かつ効果的に行うためです。

## 方法

文字列を小文字に変換するには、```(clojure.string/lower-case "HELLO WORLD")``` のように ```clojure.string/lower-case``` 関数を使用します。これにより、文字列 "HELLO WORLD" が "hello world" に変換されます。

```Clojure
;; コード例

(clojure.string/lower-case "HELLO WORLD")
;; => "hello world"

(clojure.string/lower-case "I LOVE CLOJURE")
;; => "i love clojure"
```

## 深堀り

文字列を小文字に変換すると、様々なメリットがあります。まず、文字列を比較する際に、大文字や小文字の違いを気にする必要がなくなります。また、文字列の長さを短くし、データベースやファイルの検索をより高速に行うことができます。さらに、同じ文字列であっても意味が異なる場合があるため、文字列を小文字に変換することでデータの整合性を保つことができます。

## その他の参考リンク

- [Clojure 公式ドキュメント](https://clojure.org/api/clojure.string)
- [文字列の比較における大文字と小文字の違い](https://dev.classmethod.jp/articles/strings-and-case-insensitivity-in-clojure/)
- [文字列操作に関する Tips とトリック](https://www.luminusweb.net/docs/strings.md)