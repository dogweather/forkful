---
title:                "JSONを使用する。"
html_title:           "Haskell: JSONを使用する。"
simple_title:         "JSONを使用する。"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## なぜ

JSONとは、データの交換や保存に適したフォーマットです。Haskellを使用することで、JSONデータを処理して使用する祭に便利です。

## 簡単な例

まず、HaskellのJSONライブラリである'Aeson'をインポートします。

```
import Data.Aeson
```

次に、JSONを表すHaskellのデータ型を定義します。

```
data User = User
  { name :: String
  , age :: Int
  , occupation :: String
  }
  deriving (Show, Generic)
```

そして、データ型をJSONに変換する`ToJSON`型クラスのインスタンスを定義します。

```
instance ToJSON User
```

JSONに変換したいデータを作成します。

```
user :: User
user = User "John Doe" 25 "Software Engineer"
```

最後に、`encode`関数を使ってJSONに変換します。

```
encode user
```

出力結果は以下のようになります。

```
"{\"name\":\"John Doe\",\"age\":25,\"occupation\":\"Software Engineer\"}"
```

このように、Haskellでは簡単にJSONデータを作成することができます。

## 深掘り

実際のプログラミングでは、より複雑なJSONデータを扱うことが多いでしょう。そのような場合には、レコード構文を使うことでデータ構造をより明示的に定義することができます。また、`FromJSON`型クラスを定義することで、JSONからHaskellのデータ型に変換することもできます。Haskellの詳細な文法や概念については、公式のドキュメントを参照することをお勧めします。

## 関連リンク

- [Aesonのソースコード](https://github.com/bos/aeson)
- [Haskell公式ドキュメント](https://www.haskell.org/documentation/)