---
title:    "Clojure: 文字列の大文字化"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換することの利点は何でしょうか？その理由を簡単に説明します。

## ハウツー

文字列を大文字に変換する方法をご紹介します。Clojureのコードブロックを使用して例と出力を示します。

```Clojure
(defn capitalize-string [str]
  (.toUpperCase str))
  
(capitalize-string "hello") ;=> "HELLO"
```

## 深堀り

文字列を大文字に変換するのに使用される `toUpperCase` 関数について、より詳しくご説明します。この関数はJavaのメソッドであり、文字列を大文字に変換するだけでなく、以前の文字列とは異なる新しい文字列を返します。

## 参考リンク

- [Clojureの公式ドキュメント](https://clojure.org/)
- [Javaの文法と標準ライブラリ](https://docs.oracle.com/javase/8/docs/api/)
- [文字列を大文字に変換する方法の比較](https://www.baeldung.com/java-string-to-uppercase)