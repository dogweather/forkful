---
date: 2024-01-20 17:34:55.313685-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\uFF12\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u7E4B\u304E\u5408\u308F\u305B\u308B\u3053\u3068\
  \u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30C7\u30FC\u30BF\u3092\u7D44\
  \u307F\u5408\u308F\u305B\u305F\u308A\u3001\u3088\u308A\u8907\u96D1\u306A\u30C6\u30AD\
  \u30B9\u30C8\u3092\u4F5C\u6210\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u4F7F\u7528\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.182188-07:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u3001\uFF12\u3064\u4EE5\
  \u4E0A\u306E\u6587\u5B57\u5217\u3092\u7E4B\u304E\u5408\u308F\u305B\u308B\u3053\u3068\
  \u3067\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30C7\u30FC\u30BF\u3092\u7D44\
  \u307F\u5408\u308F\u305B\u305F\u308A\u3001\u3088\u308A\u8907\u96D1\u306A\u30C6\u30AD\
  \u30B9\u30C8\u3092\u4F5C\u6210\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、２つ以上の文字列を繋ぎ合わせることです。これにより、データを組み合わせたり、より複雑なテキストを作成したりするためにプログラマーが使用します。

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
