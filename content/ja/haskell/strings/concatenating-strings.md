---
title:                "文字列の連結"
aliases:
- /ja/haskell/concatenating-strings.md
date:                  2024-01-20T17:34:55.313685-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/concatenating-strings.md"
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
