---
title:                "文字列を大文字にする"
aliases:
- /ja/haskell/capitalizing-a-string.md
date:                  2024-02-03T19:05:28.090850-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## What & Why?
文字列の先頭を大文字にし、残りの文字を小文字にすることを文字列の大文字化といいます。プログラマーは、出力のフォーマットを整えるため、テキストの文法的正確さを守るため、または生成されたデータの読みやすさを向上させるためにこれを行います。

## どのようにして：
Haskellでは、標準ライブラリを使って、サードパーティのライブラリを必要とせずに文字列を大文字化することができます。

```haskell
import Data.Char (toUpper, toLower)

capitalize :: String -> String
capitalize "" = ""
capitalize (head:tail) = toUpper head : map toLower tail

-- サンプル使用法:
main = putStrLn $ capitalize "hello world"
```

出力:
```
Hello world
```

より複雑なシナリオや使いやすさを求める場合は、Haskellで効率的な文字列操作に人気のある`text`というサードパーティライブラリの使用を検討したいかもしれません。

まず、プロジェクトの依存関係に`text`を追加する必要があります。その後、以下のようにしてその機能を使用して文字列を大文字化します：

```haskell
import qualified Data.Text as T
import Data.Char (toUpper)

capitalizeText :: T.Text -> T.Text
capitalizeText text = case T.uncons text of
    Nothing -> T.empty
    Just (first, rest) -> T.cons (toUpper first) (T.toLower rest)

-- textライブラリを使用したサンプル使用法:
main = putStrLn $ T.unpack $ capitalizeText (T.pack "hello world")
```

出力:
```
Hello world
```

これらの例は、サードパーティライブラリを使用するかどうかにかかわらず、Haskellで文字列を大文字化するためのシンプルで効果的な方法を示しています。
