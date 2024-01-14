---
title:                "Clojure: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 私たちはなぜ文字列を小文字に変換するのか

文字列を小文字に変換すると、文字列の比較や処理がより簡単になります。また、データの整理や平準化の際にも便利です。

## 方法

文字列を小文字に変換するには、`clojure.string/lower-case`関数を使用します。

```Clojure
(clojure.string/lower-case "HELLO WORLD")
```

このコードを実行すると、`hello world`という出力が得られます。

```Clojure
(clojure.string/lower-case "こんにちは、世界")
```

もちろん、日本語のようなマルチバイト文字列でも同様に動作します。このコードを実行すると、`こんにちは、世界`という出力が得られます。

## ディープダイブ

`clojure.string/lower-case`関数は、文字列を小文字に変換するだけでなく、国際化のための複雑なルールにも対応しています。例えば、トルコ語の`I`の小文字は`ı`になりますが、この関数では適切に処理されるようになっています。

また、`clojure.string/lower-case`関数は、文字列だけでなくシーケンスやマップなどのデータ構造にも適用可能です。これにより、大量のデータを効率的に小文字に変換することができます。

## 関連リンク

- [Clojure Documentation: string functions](https://clojuredocs.org/clojure.string/lower-case)
- [Clojure for Beginners - How to Convert Strings to Lowercase](https://javarevisited.blogspot.com/2020/06/clojure-convert-string-to-lowercase-uppercase.html)