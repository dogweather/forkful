---
title:    "Haskell: 文字列の大文字化"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## なぜ

文字列を大文字にすることに興味を持つ人の動機は様々ですが、例えばデータ処理や単語の正規化などでよく使われます。

## 方法

以下のコードは、Haskellで文字列を大文字にする方法を示しています。

```Haskell
import Data.Char (toUpper)

capString :: String -> String
capString str = map toUpper str

main :: IO ()
main = do
    let str = "hello, world!"
    putStrLn (capString str)

-- Output: HELLO, WORLD!
```

## 深堀り

文字列を大文字にするためには、Haskellの`map`関数を使い、文字列を文字単位で大文字に変換することができます。これにより、リストの各要素に対して関数を適用することができます。

また、文字列を単語単位で大文字にしたい場合は、`words`関数で文字列を単語のリストに分割し、`unwords`関数で単語を結合することができます。

## 参考リンク

- [Haskellで文字列を大文字にする方法 (Qiita)](https://qiita.com/AsahiTakai/items/3a736bb244c95f55db79)
- [String.capitalize() - Python 3.7.3 ドキュメント](https://docs.python.org/ja/3/library/stdtypes.html#str.capitalize)
- [Haskell文書 (HaskellWiki)](https://wiki.haskell.org/).