---
title:                "文字列から引用符を削除する"
aliases:
- /ja/haskell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:07.816814-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から引用符を削除する"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から引用符を取り除くとは、文字列データの一部として含まれる任意の引用符―シングル（' '）またはダブル（" "）―を除去することを意味します。プログラマーは、入力をサニタイズしたり、テキストを処理の準備をしたり、データの処理や操作に干渉するかもしれない不要な文字を取り除くためにこれを行います。

## 方法：
Haskellでは、与えられた文字列からすべての引用符を取り除く関数を簡単に作成できます。これは、引用符に去るように言い、それがヒントを受け取ることを確実にします。

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell said, \"Let's learn some functions!\""
    putStrLn $ removeQuotes stringWithQuotes
```

サンプル出力：

```
Haskell said, Lets learn some functions!
```

## 詳細解説
かつて、プログラミングにおいて文字列がインターネット上の猫の動画ほど一般的になる前は、テキストを扱うことが難しいビジネスでした。しかし、プログラミング言語が進化するにつれて、文字列はコーディングの重要な部分となりました。それでも、引用符は依然として両刃の剣でした―文字列を定義するためには不可欠ですが、実際のデータとして含まれる場合は迷惑でした。

代わりに？すべての引用符をハエのように払いのける代わりに、選択的に行うことができます。最も外側の引用符のみを取り除く（クラシックなトリム）や、文字列内のエスケープされた引用符を扱うことができるかもしれません。

実装上、上記の`removeQuotes`関数は、各文字(`c`)が厄介な引用符であるかどうかをチェックするためにラムダを使用し、それに応じてフィルタリングします。これは直接的なアプローチですが、より大きなテキストや複雑なルールのためには、テキスト処理においてより洗練されたパワーを提供できる`Parsec`のようなパーサーライブラリを検討することをお勧めします。

## 参照：
- 正規表現好きのために：[Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Haskell文字列への優しい入門：[Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
