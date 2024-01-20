---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Haskell文字列を小文字に変換する方法

## 何となぜ？
文字列の変換は、一部または全体を特定の形式にするために使用します。例えば、文字列を全て小文字に変換することは、ケース無視の文字列比較や検索に役立ちます。

## どうやって：
以下の関数`toLowerStr`を使用します。この関数は、文字列全体を小文字に変換します。

```Haskell
import Data.Char

toLowerStr :: String -> String
toLowerStr = map toLower
```

これを実行すると次のようになります：

```Haskell
main = print(toLowerStr "Hello World!")
-- 出力: "hello world!"
```

## ディープダイブ：
文字列を小文字に変換する方法は長い歴史があり、多くの方法が存在します。Haskellでは、文字単位で操作を行って文字列全体を変換します。`map`関数と`toLower`関数の組み合わせが一般的です。

また、一部の文字列だけを小文字に変換するなど、より細かい制御が必要な場合もあります。そのような場合には、`span`, `break`, `splitAt`などの関数を使用します。

Haskellの`toLower`関数は`Data.Char`モジュールに定義されており、Unicode文字をサポートしています。そのため、多言語のテキストでも適切に動作します。

エラーハンドリングについては、この関数は安全です。無効な文字を変換しようとすると、その文字は単に無視されます。

## 参考文献
以下のリンクでは、Haskellでの文字列操作についてさらに詳しく知ることができます：

1. [Haskell Wiki - String Conversion](https://wiki.haskell.org/String_conversion)
2. [Haskell Library - Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
3. [Haskell Documentation - map function](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:map)