---
title:                "文字列の連結"
html_title:           "Bash: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列連結は、二つ以上の文字列を一つに結びつけるプログラミングの操作です。これは異なるソースからのデータを一緒に表示するためや、パスを作成するためによく使われます。

## どうやって：

Haskellでは、`++` 演算子を使用して文字列を連結します。

```Haskell
main = do
  let str1 = "こんにちは、"
  let str2 = "世界！"
  putStrLn (str1 ++ str2)
```

実行すると、`こんにちは、世界！`と出力されます。

## ディープダイブ：

Haskellでの文字列連結の歴史は、機能とパフォーマンスを考慮に入れて、時間の経過とともに発展してきました。初期のアプローチは、リストの連結と同様に扱われていました。

Haskellでは、`++`以外にも`concat`関数を使用して文字列を連結することができます。

```Haskell
main = do
  let strs = ["こんにちは、", "世界！"]
  putStrLn (concat strs)
```

これもまた、`こんにちは、世界！`と出力されます。

ただし、Haskellでは文字列は文字のリストとして扱われており、大量のデータを扱う場合にパフォーマンスに問題が生じる可能性があるため注意が必要です。

## 関連資料：

文字列連結について更に学びたい方は、以下のリンクをご覧ください：
- [Haskellでの文字列操作](http://learnyouahaskell.com/starting-out#strings)
- [Haskellプログラミング](https://www.haskell.org/tutorial/strings.html)
- [Haskellで文字列連結を最適化する](https://stackoverflow.com/questions/31279545/why-is-string-concatenation-so-slow-in-haskell)