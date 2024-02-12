---
title:                "部分文字列の抽出"
aliases:
- /ja/haskell/extracting-substrings/
date:                  2024-01-20T17:46:12.004589-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列から部分文字列を取り出すとは、大きな文字列の中から必要な範囲の文字だけを抜き出すことです。この操作はデータの解析、パースや、特定のパターン検索時などによく使われます。

## How to: (方法)
Haskellでは部分文字列の抽出には様々な関数が用意されています。以下に例を示します。

```haskell
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)

main :: IO ()
main = do
  let text = "こんにちはHaskell世界"

  -- 先頭からの取り出し
  let beginning = take 5 text
  putStrLn beginning  -- 出力: "こんにちは"

  -- 末尾からの取り出し
  let ending = drop 5 text
  putStrLn ending  -- 出力: "Haskell世界"

  -- 部分文字列が存在するか確認
  putStrLn $ if "Haskell" `isInfixOf` text then "含む" else "含まない" -- 出力: "含む"

  -- パターンにマッチする部分を取り出し（先頭マッチ）
  putStrLn $ if "こんにちは" `isPrefixOf` text then "マッチする" else "マッチしない" -- 出力: "マッチする"

  -- パターンにマッチする部分を取り出し（末尾マッチ）
  putStrLn $ if "世界" `isSuffixOf` text then "マッチする" else "マッチしない" -- 出力: "マッチする"
```

## Deep Dive (深堀り)
部分文字列を取り出す処理はHaskellの初期からある基本的な機能です。`take`、`drop`関数はリスト操作関数で、文字列は文字のリストとして扱えるため、これらを直接使えます。代替として `takeWhile` や `dropWhile` 関数もあり、条件を満たす間要素を取り出すことができます。

Haskellには文字列専用の便利なライブラリもあるため、例えば `Data.Text` ライブラリだとより強力な文字列処理関数を使えます。これらは `Text` 型が必要ですが、`pack` と `unpack` 関数で通常の文字列と変換可能です。

パフォーマンス面では、文字列を頻繁に操作する場合は `Text` 型や `ByteString` 型を使う方が効率的です。これはHaskellのデフォルト文字列（リスト）が連結リストであるため、大きなデータには不向きだからです。

## See Also (参照)
1. Real World Haskell: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)
2. Haskell `Data.Text` documentation: [https://hackage.haskell.org/package/text](https://hackage.haskell.org/package/text)
3. Learn You a Haskell for Great Good: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
4. Haskell `Data.ByteString` documentation: [https://hackage.haskell.org/package/bytestring](https://hackage.haskell.org/package/bytestring)
