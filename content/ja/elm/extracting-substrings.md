---
title:                "サブ文字列の抽出"
html_title:           "Elm: サブ文字列の抽出"
simple_title:         "サブ文字列の抽出"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

##どういうことか？
文字列から部分文字列を抽出することは、文字列の一部を別の文字列として取り出すことを意味します。プログラマーたちは、特定のテキストを処理する際に、必要な情報を抽出するためにこの機能を使用します。

##やり方：
```Elm 「 extractSubstrings "hey there" 1 4-- "ey t"」 
 ``` 
以下のように実行されます: 
「 eyt 」

```Elm listOne = ["a", "b", "c", "d", "e"]
listTwo = ["a", "b"]
newList = List.filter (\x -> List.contains x listTwo) listOne

「 newList 」 
 ``` 
以下のように実行されます: 
```Elm ["a", "b"]
 ```

##深く掘り下げる：
文字列から部分文字列を抽出する概念は、古くから存在しており、プログラミング言語の多くに組み込まれています。代替手段としては、正規表現を使用したパターンマッチングがあります。Elmで部分文字列を抽出する場合、Stringモジュールの「slice」関数を使用します。第一引数には元の文字列、第二引数には抽出したい部分文字列の開始位置、第三引数には終了位置を指定します。

##参考文献：
- [Elm - Syntax](https://elm-lang.org/docs/syntax#lists)
- [Elm - String Module](https://package.elm-lang.org/packages/elm/core/latest/String)