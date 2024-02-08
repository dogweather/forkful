---
title:                "YAML を操作する"
aliases:
- ja/haskell/working-with-yaml.md
date:                  2024-02-03T19:25:54.908090-07:00
model:                 gpt-4-0125-preview
simple_title:         "YAML を操作する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
