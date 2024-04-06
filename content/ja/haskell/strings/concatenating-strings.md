---
date: 2024-01-20 17:34:55.313685-07:00
description: "How to: (\u65B9\u6CD5) Haskell\u3067\u6587\u5B57\u5217\u3092\u9023\u7D50\
  \u3059\u308B\u306E\u306F\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002\u6A19\u6E96\u7684\
  \u306A\u65B9\u6CD5\u306F`(++)`\u30AA\u30DA\u30EC\u30FC\u30BF\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u4F8B\u3067\u3059\
  \uFF1A."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.040595-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Haskell\u3067\u6587\u5B57\u5217\u3092\u9023\u7D50\u3059\u308B\
  \u306E\u306F\u30B7\u30F3\u30D7\u30EB\u3067\u3059\u3002\u6A19\u6E96\u7684\u306A\u65B9\
  \u6CD5\u306F`(++)`\u30AA\u30DA\u30EC\u30FC\u30BF\u3092\u4F7F\u7528\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u305D\u306E\u4F8B\u3067\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (方法)
Haskellで文字列を連結するのはシンプルです。標準的な方法は`(++)`オペレータを使用することです。以下はその例です：

```Haskell
main :: IO ()
main = do
  let hello = "こんにちは、"
  let world = "世界！"
  putStrLn (hello ++ world)
```

出力は次のようになります：

```
こんにちは、世界！
```

他の方法として、`concat`関数やリスト内包表記を利用することもできます。

```Haskell
main :: IO ()
main = do
  let phrases = ["Haskell ", "は ", "素晴らしい ", "です！"]
  putStrLn (concat phrases)

  let part1 = "Haskell"
  let part2 = "まじ"
  putStrLn ([part1, part2, "？"] >>= id)
```

出力：

```
Haskell は 素晴らしい です！
Haskellまじ？
```

## Deep Dive (深掘り)
Haskellの文字列は、単に文字のリストとして表されます。そのため、リストを連結する過程と同様です。例えば、`(++)`オペレータは、背後では１つ目のリストの末尾に到達するまで再帰的に処理を行います。

過去、文字列を効率的に扱うため、`Data.Text` モジュールが導入されました。このモジュールでは、より効率的な文字列操作が可能ですが、上記の例では標準の文字列型を使用しています。

代わりに `Data.Text` を使用すると、メモリ効率とパフォーマンスが向上しますが、プログラムのサイズが大きくなる場合があります。

```Haskell
import qualified Data.Text as T

main :: IO ()
main = do
  let hello = T.pack "こんにちは、"
  let world = T.pack "世界！"
  T.putStrLn (T.append hello world)
```

`Data.Text`は日常的には `(++)` の代わりに `append` 関数を提供します。

## See Also (関連情報)
- Haskellの公式ドキュメント: https://www.haskell.org/documentation
- `Data.Text`モジュールドキュメント: https://hackage.haskell.org/package/text
- Learn You a Haskell for Great Good! オンライン書籍: http://learnyouahaskell.com/chapters
