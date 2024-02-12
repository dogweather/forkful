---
title:                "正規表現の使用"
aliases:
- /ja/haskell/using-regular-expressions.md
date:                  2024-02-03T19:17:01.958341-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
プログラミングで使われる正規表現は、文字列の検索パターンを定義する文字の連続であり、通常、文字列検索や操作のために用いられます。Haskell プログラマーは、簡単な文字列マッチングから複雑なテキスト処理まで、その効率性とテキストデータを扱う際の汎用性を活かして正規表現を利用します。

## 使い方：
Haskellでは、regex機能は標準ライブラリには含まれておらず、`regex-base`のようなサードパーティ製のパッケージと、POSIX正規表現サポート用の`regex-posix`、Perl互換の正規表現用の`regex-pcre`など、互換性のあるバックエンドの使用が必要になります。以下のようにしてこれらのパッケージを使用して正規表現を扱います。

まず、プロジェクトの `.cabal` ファイルに `regex-posix` や `regex-pcre` を追加するか、cabalを直接使ってパッケージをインストールすることで、パッケージがインストールされていることを確認します：

```bash
cabal install regex-posix
```
または
```bash
cabal install regex-pcre
```

### `regex-posix` を使う：

```haskell
import Text.Regex.Posix ((=~))

-- 文字列がパターンにマッチするか確認
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- 最初のマッチを見つける
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- 出力: True
    print $ findFirst "good morning, good night" "good"
    -- 出力: "good"
```

### `regex-pcre` を使う：

```haskell
import Text.Regex.PCRE ((=~))

-- すべてのマッチを見つける
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- 出力: ["test1","test2","test3"]
```

各ライブラリには特有の点がありますが、regexを適用するために `=~` を使用する一般的な方法論は一貫しています。マッチの確認か部分文字列の抽出かにかかわらず、`regex-posix` と `regex-pcre` のどちらを選ぶかは、主にプロジェクトのニーズと必要な特定のregex機能に依存します。
