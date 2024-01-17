---
title:                "「Yamlを使う」"
html_title:           "Elm: 「Yamlを使う」"
simple_title:         "「Yamlを使う」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## これは何というものか？

YAMLとは、人間が読みやすい形式でデータを表現するための言語です。プログラマーは、YAMLを使用してコンフィグファイルやデータ処理に関する情報を整理、保存、共有することができます。

## 使い方：

```Elm
-- 例1：コンフィグファイルを読み込む
import Yaml.Decode as Decode

type alias Config =
  { username : String
  , password : String
  }

config : Decode.Decoder Config
config =
  Decode.succeed Config
    |> Decode.required "username" Decode.string
    |> Decode.required "password" Decode.string

loadConfig : String -> Result String Config
loadConfig filePath =
  Decode.decodeString config filePath

-- 例2：データをYAML形式で保存する
import Yaml.Encode as Encode

data : List String
data =
  ["apple", "banana", "orange"]

encodeData : String
encodeData =
  Encode.encode 2 data

-- 出力：
-- "- apple
--   - banana
--   - orange"

```

## より詳しく：

YAMLは、XMLやJSONと同様に、データの構造化と交換を目的として作られました。XMLよりもヒューマンリーダブルな構文を持ち、JSONよりもさらに簡潔にデータを表現することができます。代替手段として、XMLやJSONを使用することもできますが、YAMLはデータの記述をよりシンプルにすることができます。また、Elm以外にも、PythonやRubyなどでも使用することができます。

## 関連情報を確認する：

- Elmの公式ドキュメント: https://elm-lang.org/docs/syntax#yamls
- YAML公式サイト: https://yaml.org/
- YAMLスニペット集: https://github.com/kesselborn/elm-yaml-snippets