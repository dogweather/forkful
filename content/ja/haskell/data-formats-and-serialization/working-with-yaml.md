---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:54.908090-07:00
description: "\u65B9\u6CD5 Haskell\u306B\u306FYAML\u51E6\u7406\u306E\u305F\u3081\u306E\
  \u7D44\u307F\u8FBC\u307F\u30B5\u30DD\u30FC\u30C8\u306F\u3042\u308A\u307E\u305B\u3093\
  \u304C\u3001`yaml`\u3084`aeson`\u306E\u3088\u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\
  \u30C6\u30A3\u88FD\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066\
  YAML\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3068\u751F\u6210\u3092\u884C\u3046\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u59CB\u3081\u308B\
  \u65B9\u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.074838-06:00'
model: gpt-4-0125-preview
summary: ''
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
