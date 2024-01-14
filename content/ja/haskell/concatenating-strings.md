---
title:                "Haskell: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ
文字列を結合することに対して、なぜ私たちは関わる必要があるのか。この記事ではその理由を紹介します。

## 方法
文字列を結合する方法について、コーディング例とサンプルの出力を示します。"```Haskell ... ```"コードブロック内に記載されます。

```Haskell
concatenateStrings :: String -> String -> String
-- concatenateStrings関数は2つの文字列を結合し、新しい文字列を作成します
concatenateStrings str1 str2 = str1 ++ str2

-- 例1:
concatenateStrings "Hello " "World!" -- 出力: "Hello World!"

-- 例2:
concatenateStrings "今日は" "いい天気ですね。" -- 出力: "今日はいい天気ですね。"
```

## ディープダイブ
文字列を結合するには、Haskellには2つの主要な方法があります。1つは `++` 演算子を用いて文字列を結合する方法です。もう一つは `concat` 関数を用いて文字列を結合する方法です。しかし、 `concat` 関数を用いる場合は文字列のリストが必要です。

## See Also
- Data.Listモジュール (https://www.haskell.org/tutorial/characters.html)
- Stringモジュール (https://www.haskell.org/tutorial/strings.html)