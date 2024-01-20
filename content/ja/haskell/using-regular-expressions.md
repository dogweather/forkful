---
title:                "正規表現を使用する"
html_title:           "Haskell: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何 && なぜ？
正規表現を使用するとは、パターンにマッチするテキストを検索したり、置き換えたりすることができるプログラミング技術のことです。プログラマーは、テキスト処理やデータ検索において、より柔軟かつ効率的な方法を求めて正規表現を使用します。

## 方法：
```Haskell
import Text.Regex.Posix

-- 文字列が正規表現にマッチするかどうかをチェックする例
match :: String -> String -> Bool
match str regex = str =~ regex

-- マッチした部分を抽出する例
extract :: String -> String -> [String]
extract str regex = getAllTextMatches (str =~ regex :: AllTextMatches [] String)

-- マッチした部分を置き換える例
replace :: String -> String -> String -> String
replace str regex replaceWith = subRegex (str =~ regex :: AllTextMatches [] String) regex replaceWith
```

### 出力：
```Haskell
> match "Hello world" "Hello" -- True
> match "Hello world" "foo" -- False
> extract "abc123def456" "[0-9]+" -- ["123","456"]
> replace "My name is John" "John" "Bob" -- "My name is Bob"
```

## 深堀り：
1. 正規表現は、1951年に計算機科学者のKen Thompsonによって発明されました。当初はUNIXのテキスト処理ツールとして使用されていましたが、今ではほとんどのプログラミング言語でサポートされています。
2. 正規表現以外にも、パターンマッチングのためのパッケージやライブラリがあります。しかし、正規表現はパターンの検索や置き換えのために最も一般的に使用されるツールです。
3. 実装の詳細としては、正規表現は有限オートマトンと呼ばれる機械の理論を基にしています。これはパターンマッチングをより効率的に行うための方法です。


## 関連リンク：
- [正規表現入門 (wikipedia)](https://ja.wikipedia.org/wiki/%E6%AD%A3%E8%A6%8F%E8%A1%A8%E7%8F%BE)
- [正規表現チュートリアル (qiita)](https://qiita.com/jnchito/items/57ffda5712636a9a1e62)