---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:09.255032-07:00
description: "\u65B9\u6CD5\uFF1A \u307E\u305A\u3001`.cabal`\u30D5\u30A1\u30A4\u30EB\
  \u3092\u66F4\u65B0\u3059\u308B\u304B\u3001Stack\u3084Cabal\u3092\u76F4\u63A5\u4F7F\
  \u7528\u3057\u3066\u3001\u30D7\u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u4F9D\u5B58\u95A2\
  \u4FC2\u306BAeson\u3092\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:43.076154-06:00'
model: gpt-4-0125-preview
summary: ''
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
