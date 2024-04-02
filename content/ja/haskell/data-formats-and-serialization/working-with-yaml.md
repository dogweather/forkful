---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:54.908090-07:00
description: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3042\u308A\u3001\u3059\u3079\u3066\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u8A00\u8A9E\u3067\u4F7F\u7528\u3067\u304D\u308B\u4EBA\u9593\u306B\u512A\u3057\u3044\
  \u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u6A19\
  \u6E96\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u306E\
  \u8AAD\u307F\u3084\u3059\u3055\u3068\u76F4\u63A5\u7684\u306A\u69CB\u9020\u306E\u305F\
  \u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3084\u8A00\u8A9E\u9593\u306E\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3067YAML\u3092\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.213935-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F\u3001\u300CYAML Ain't Markup Language\u300D\u306E\u7565\u3067\
  \u3042\u308A\u3001\u3059\u3079\u3066\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\
  \u8A00\u8A9E\u3067\u4F7F\u7528\u3067\u304D\u308B\u4EBA\u9593\u306B\u512A\u3057\u3044\
  \u30C7\u30FC\u30BF\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u6A19\
  \u6E96\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u305D\u306E\
  \u8AAD\u307F\u3084\u3059\u3055\u3068\u76F4\u63A5\u7684\u306A\u69CB\u9020\u306E\u305F\
  \u3081\u3001\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3084\u8A00\u8A9E\u9593\u306E\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3067YAML\u3092\u3088\u304F\u4F7F\u7528\u3057\u307E\u3059\
  \u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

## 何となぜ？

YAMLは、「YAML Ain't Markup Language」の略であり、すべてのプログラミング言語で使用できる人間に優しいデータシリアライゼーション標準です。プログラマーは、その読みやすさと直接的な構造のため、設定ファイルや言語間のデータ交換でYAMLをよく使用します。

## 方法

HaskellにはYAML処理のための組み込みサポートはありませんが、`yaml`や`aeson`のようなサードパーティ製のライブラリを使用してYAMLデータの解析と生成を行うことができます。以下は、始める方法です：

### YAMLの読み取り
まず、プロジェクトの依存関係に`yaml`パッケージを追加します。次に、以下の例を使用してシンプルなYAMLドキュメントを解析できます：

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (liftIO)

-- 例 YAML データ
yamlData :: ByteString
yamlData = "
name: John Doe
age: 30
"

-- YAMLドキュメントに一致するデータ構造を定義
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show)

instance FromYAML Person where
  parseYAML = withMap "Person" $ \m -> Person
    <$> m .: "name"
    <*> m .: "age"

main :: IO ()
main = do
  let parsed = decode1 yamlData :: Either (Pos,String) Person
  case parsed of
    Left err -> putStrLn $ "YAMLの解析エラー: " ++ show err
    Right person -> print person
```
上記のコードのサンプル出力は次のようになるかもしれません:
```
Person {name = "John Doe", age = 30}
```

### YAMLの書き込み
Haskellのデータ構造からYAMLを生成するには、以下に示すように、`yaml`パッケージのエンコーディング機能を使用できます：

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.YAML
import Data.ByteString.Lazy.Char8 (unpack)

-- 前の例からのPersonデータ構造を使用

person :: Person
person = Person "Jane Doe" 25

main :: IO ()
main = do
  let yamlData = encode1 person
  putStrLn $ unpack yamlData
```
このプログラムの出力は、YAML形式の文字列になります：
```
name: Jane Doe
age: 25
```

これらの例は、HaskellでYAMLを扱うための出発点として機能するはずです。ニーズに応じて、これらのライブラリが提供するより高度な機能やオプションを探索したい場合があるでしょう。
