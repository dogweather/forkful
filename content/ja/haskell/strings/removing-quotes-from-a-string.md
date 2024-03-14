---
date: 2024-01-26 03:40:07.816814-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u306F\u3001\u6587\u5B57\u5217\u30C7\u30FC\u30BF\u306E\u4E00\u90E8\u3068\
  \u3057\u3066\u542B\u307E\u308C\u308B\u4EFB\u610F\u306E\u5F15\u7528\u7B26\u2015\u30B7\
  \u30F3\u30B0\u30EB\uFF08' '\uFF09\u307E\u305F\u306F\u30C0\u30D6\u30EB\uFF08\" \"\
  \uFF09\u2015\u3092\u9664\u53BB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u3092\u30B5\
  \u30CB\u30BF\u30A4\u30BA\u3057\u305F\u308A\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u51E6\
  \u7406\u306E\u6E96\u5099\u3092\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u306E\u51E6\
  \u7406\u3084\u64CD\u4F5C\u306B\u5E72\u6E09\u3059\u308B\u304B\u3082\u3057\u308C\u306A\
  \u3044\u4E0D\u8981\u306A\u6587\u5B57\u3092\u53D6\u308A\u9664\u304F\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.165672-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u53D6\u308A\u9664\
  \u304F\u3068\u306F\u3001\u6587\u5B57\u5217\u30C7\u30FC\u30BF\u306E\u4E00\u90E8\u3068\
  \u3057\u3066\u542B\u307E\u308C\u308B\u4EFB\u610F\u306E\u5F15\u7528\u7B26\u2015\u30B7\
  \u30F3\u30B0\u30EB\uFF08' '\uFF09\u307E\u305F\u306F\u30C0\u30D6\u30EB\uFF08\" \"\
  \uFF09\u2015\u3092\u9664\u53BB\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u5165\u529B\u3092\u30B5\
  \u30CB\u30BF\u30A4\u30BA\u3057\u305F\u308A\u3001\u30C6\u30AD\u30B9\u30C8\u3092\u51E6\
  \u7406\u306E\u6E96\u5099\u3092\u3057\u305F\u308A\u3001\u30C7\u30FC\u30BF\u306E\u51E6\
  \u7406\u3084\u64CD\u4F5C\u306B\u5E72\u6E09\u3059\u308B\u304B\u3082\u3057\u308C\u306A\
  \u3044\u4E0D\u8981\u306A\u6587\u5B57\u3092\u53D6\u308A\u9664\u304F\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
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
