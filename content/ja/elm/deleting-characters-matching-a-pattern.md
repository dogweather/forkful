---
title:                "パターンに一致する文字を削除する"
html_title:           "Elm: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ?
文字列のパターンにマッチする文字を削除することは、プログラマーがよく行う作業です。これには、余分な文字を削除したり、不要な情報を取り除いたりするための目的があります。

## 方法:
```Elm
deletePattern : String -> String -> String
deletePattern pattern str =
    String.replace pattern "" str
```

入力された文字列から、指定したパターンにマッチする文字を除外します。例えば、```deletePattern "e" "elm"```を実行すると、"lm"が返されます。

## 深堀り:
- 歴史的背景:
文字列のパターンを削除する作業は、文書処理やテキスト解析などの分野で古くから行われてきました。プログラミング言語における実装もさまざまありますが、Elmでは```String.replace```関数を使用することで簡単に実現できます。
- 代替手段:
文字列のパターンを削除する方法は、他にも多く存在します。例えば、正規表現や自作のアルゴリズムを用いることもできますが、Elmの場合は```String.replace```が簡単で効率的な方法です。
- 実装の詳細:
Elmの```String.replace```関数は、Haskellの関数を参考にして実装されています。文字列のパターンマッチングには、KMPアルゴリズムが使われています。

## 関連リンク:
- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [KMP Algorithm Explanation](https://www.geeksforgeeks.org/kmp-algorithm-for-pattern-searching/)
- [Haskell String Replace Implementation](https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-String.html#v:replace)