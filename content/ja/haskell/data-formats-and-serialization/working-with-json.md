---
title:                "JSONを活用する"
aliases:
- /ja/haskell/working-with-json/
date:                  2024-02-03T19:23:09.255032-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
HaskellでのJSON（JavaScript Object Notation）の取り扱いは、JSONデータをHaskellの型に解析（パース）し、Haskellの型をJSONに変換することを含みます。プログラマーは、これを行うことで、HaskellアプリケーションがWebサービスやAPIとシームレスにデータ交換できるようにします。これは、クロスプラットフォームのデータ交換のために、現代のソフトウェア開発で一般的な実践です。

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
