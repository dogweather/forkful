---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:01.958341-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u4F7F\u308F\u308C\u308B\
  \u6B63\u898F\u8868\u73FE\u306F\u3001\u6587\u5B57\u5217\u306E\u691C\u7D22\u30D1\u30BF\
  \u30FC\u30F3\u3092\u5B9A\u7FA9\u3059\u308B\u6587\u5B57\u306E\u9023\u7D9A\u3067\u3042\
  \u308A\u3001\u901A\u5E38\u3001\u6587\u5B57\u5217\u691C\u7D22\u3084\u64CD\u4F5C\u306E\
  \u305F\u3081\u306B\u7528\u3044\u3089\u308C\u307E\u3059\u3002Haskell \u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7C21\u5358\u306A\u6587\u5B57\u5217\u30DE\u30C3\
  \u30C1\u30F3\u30B0\u304B\u3089\u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\
  \u307E\u3067\u3001\u305D\u306E\u52B9\u7387\u6027\u3068\u30C6\u30AD\u30B9\u30C8\u30C7\
  \u30FC\u30BF\u3092\u6271\u3046\u969B\u306E\u6C4E\u7528\u6027\u3092\u6D3B\u304B\u3057\
  \u3066\u6B63\u898F\u8868\u73FE\u3092\u5229\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.168911-06:00'
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u3067\u4F7F\u308F\u308C\u308B\
  \u6B63\u898F\u8868\u73FE\u306F\u3001\u6587\u5B57\u5217\u306E\u691C\u7D22\u30D1\u30BF\
  \u30FC\u30F3\u3092\u5B9A\u7FA9\u3059\u308B\u6587\u5B57\u306E\u9023\u7D9A\u3067\u3042\
  \u308A\u3001\u901A\u5E38\u3001\u6587\u5B57\u5217\u691C\u7D22\u3084\u64CD\u4F5C\u306E\
  \u305F\u3081\u306B\u7528\u3044\u3089\u308C\u307E\u3059\u3002Haskell \u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u7C21\u5358\u306A\u6587\u5B57\u5217\u30DE\u30C3\
  \u30C1\u30F3\u30B0\u304B\u3089\u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u51E6\u7406\
  \u307E\u3067\u3001\u305D\u306E\u52B9\u7387\u6027\u3068\u30C6\u30AD\u30B9\u30C8\u30C7\
  \u30FC\u30BF\u3092\u6271\u3046\u969B\u306E\u6C4E\u7528\u6027\u3092\u6D3B\u304B\u3057\
  \u3066\u6B63\u898F\u8868\u73FE\u3092\u5229\u7528\u3057\u307E\u3059\u3002."
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
