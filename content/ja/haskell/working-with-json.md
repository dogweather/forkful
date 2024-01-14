---
title:                "Haskell: 「jsonを使ったプログラミング」"
simple_title:         "「jsonを使ったプログラミング」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ
あなたがHaskellでJSONを操作することに興味があるかもしれません。JSONはWeb開発において非常にポピュラーなデータフォーマットであり、Haskellにおいても優れたパーサーライブラリが存在します。

## 作り方
最初に、HaskellのパッケージマネージャーであるStackをインストールします。その後、以下のコードを実行することで、HaskellのデフォルトのコンパイラであるGHCとJSONパーサーライブラリであるAesonがインストールされます。

```
stack install ghc
stack install aeson
```

次に、Haskellファイルを作成し、Aesonパッケージをインポートします。

```
import Data.Aeson
```

そして、JSONファイルをパースする関数を作成し、パース結果を任意のデータ型に変換します。

```
parseJSON :: Text -> Maybe MyDataType
parseJSON jsonString = decode jsonString :: Maybe MyDataType
```

最後に、作成した関数を利用してJSONファイルをパースし、結果を出力します。

```
main = do
  let jsonFile = "sample.json"
  jsonString <- readFile jsonFile
  let parsedData = parseJSON jsonString
  print parsedData
```

実行結果は以下のようになります。

```
Just MyData { id = 123, name = "Haskell", description = "Functional programming language" }
```

## 深堀り
より詳細な情報を知りたい方は、Aesonのドキュメンテーションを参照することをお勧めします。Aesonは高速で強力なパーサーライブラリであり、JSONをHaskellのデータ型に変換する際にも柔軟性があります。また、Aesonを利用することで、JSONの生成や変更も簡単に行うことができます。

## 参考
- [Aesonのドキュメンテーション](https://hackage.haskell.org/package/aeson)
- [HaskellのStackの公式サイト](https://docs.haskellstack.org/en/stable/README/)