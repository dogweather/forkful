---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:09.255032-07:00
description: "Haskell\u3067\u306EJSON\uFF08JavaScript Object\u2026"
lastmod: '2024-03-13T22:44:42.215346-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u3067\u306EJSON\uFF08JavaScript Object Notation\uFF09\u306E\u53D6\
  \u308A\u6271\u3044\u306F\u3001JSON\u30C7\u30FC\u30BF\u3092Haskell\u306E\u578B\u306B\
  \u89E3\u6790\uFF08\u30D1\u30FC\u30B9\uFF09\u3057\u3001Haskell\u306E\u578B\u3092\
  JSON\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u3053\u308C\u3092\u884C\u3046\u3053\
  \u3068\u3067\u3001Haskell\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u304C\
  Web\u30B5\u30FC\u30D3\u30B9\u3084API\u3068\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u3067\u304D\u308B\u3088\u3046\u306B\u3057\u307E\u3059\u3002\
  \u3053\u308C\u306F\u3001\u30AF\u30ED\u30B9\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\
  \u30E0\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u306E\u305F\u3081\u306B\u3001\u73FE\u4EE3\
  \u306E\u30BD\u30D5\u30C8\u30A6\u30A7\u30A2\u958B\u767A\u3067\u4E00\u822C\u7684\u306A\
  \u5B9F\u8DF5\u3067\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 方法：
HaskellはJavaScriptのようにJSONをサポートする組み込み機能を持っていませんが、**Aeson**などのサードパーティ製ライブラリの助けを借りると、JSONの取り扱いが直接的になります。Aesonは、エンコーディング（Haskellの値をJSONに変換）とデコーディング（JSONをHaskellの値に解析）のための高レベルと低レベルの両方の関数を提供します。

### Aesonのインストール
まず、`.cabal`ファイルを更新するか、StackやCabalを直接使用して、プロジェクトの依存関係にAesonを追加します：

```shell
cabal update && cabal install aeson
```
または、Stackを使用している場合：
```shell
stack install aeson
```

### JSONの解析
JSONデータをHaskellの型にデコードする基本的な例から始めましょう。次のような人物を表すJSONを持っているとします：

```json
{
  "name": "John Doe",
  "age": 30
}
```

最初に、対応するHaskellのデータ型を定義し、`FromJSON`のインスタンスにします：

```haskell
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, decode)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Generic, Show)

instance FromJSON Person

-- ファイルからJSONをデコードする関数
decodePerson :: FilePath -> IO (Maybe Person)
decodePerson filePath = do
  personJson <- B.readFile filePath
  return $ decode personJson
```
使用方法：
上に示されたJSONデータが`person.json`に含まれていると仮定し、実行します：
```haskell
main :: IO ()
main = do
  maybePerson <- decodePerson "person.json"
  print maybePerson
```
サンプル出力：
```haskell
Just (Person {name = "John Doe", age = 30})
```

### Haskellの値をJSONとしてエンコード
Haskellの値をJSONに戻すには、タイプを`ToJSON`のインスタンスにしてから、`encode`を使用します。

```haskell
import Data.Aeson (ToJSON, encode)
import GHC.Generics (Generic)

-- 前述のPersonタイプを想定

instance ToJSON Person

encodePerson :: Person -> B.ByteString
encodePerson = encode

main :: IO ()
main = do
  let person = Person "Jane Doe" 32
  putStrLn $ show $ encodePerson person
```
サンプル出力：
```json
{"name":"Jane Doe","age":32}
```

これらの例は、Aesonを使用してHaskellでJSONを扱う基本を示しています。Aesonは、カスタム解析ルール、複雑なネステッドJSONの取り扱い、さまざまなニーズとシナリオに適した多くのものを提供していることを覚えておいてください。
