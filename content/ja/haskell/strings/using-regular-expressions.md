---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:01.958341-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Haskell\u3067\u306F\u3001regex\u6A5F\u80FD\u306F\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u306F\u542B\u307E\u308C\u3066\u304A\
  \u3089\u305A\u3001`regex-base`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u88FD\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3068\u3001POSIX\u6B63\u898F\
  \u8868\u73FE\u30B5\u30DD\u30FC\u30C8\u7528\u306E`regex-posix`\u3001Perl\u4E92\u63DB\
  \u306E\u6B63\u898F\u8868\u73FE\u7528\u306E`regex-\u2026"
lastmod: '2024-04-05T22:38:41.709804-06:00'
model: gpt-4-0125-preview
summary: "\u4F7F\u3044\u65B9\uFF1A Haskell\u3067\u306F\u3001regex\u6A5F\u80FD\u306F\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u306F\u542B\u307E\u308C\u3066\u304A\
  \u3089\u305A\u3001`regex-base`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u88FD\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3068\u3001POSIX\u6B63\u898F\
  \u8868\u73FE\u30B5\u30DD\u30FC\u30C8\u7528\u306E`regex-posix`\u3001Perl\u4E92\u63DB\
  \u306E\u6B63\u898F\u8868\u73FE\u7528\u306E`regex-pcre`\u306A\u3069\u3001\u4E92\u63DB\
  \u6027\u306E\u3042\u308B\u30D0\u30C3\u30AF\u30A8\u30F3\u30C9\u306E\u4F7F\u7528\u304C\
  \u5FC5\u8981\u306B\u306A\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u3088\u3046\u306B\
  \u3057\u3066\u3053\u308C\u3089\u306E\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u4F7F\u7528\
  \u3057\u3066\u6B63\u898F\u8868\u73FE\u3092\u6271\u3044\u307E\u3059\u3002 \u307E\u305A\
  \u3001\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E `.cabal` \u30D5\u30A1\u30A4\u30EB\
  \u306B `regex-posix` \u3084 `regex-pcre` \u3092\u8FFD\u52A0\u3059\u308B\u304B\u3001\
  cabal\u3092\u76F4\u63A5\u4F7F\u3063\u3066\u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u30A4\
  \u30F3\u30B9\u30C8\u30FC\u30EB\u3059\u308B\u3053\u3068\u3067\u3001\u30D1\u30C3\u30B1\
  \u30FC\u30B8\u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u3044\u308B\
  \u3053\u3068\u3092\u78BA\u8A8D\u3057\u307E\u3059\uFF1A."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
