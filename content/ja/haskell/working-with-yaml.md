---
title:                "YAMLを使用する"
html_title:           "Haskell: YAMLを使用する"
simple_title:         "YAMLを使用する"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## なに？どんなふうにするの？
YAML (ヤムル) とは、プログラマーがデータを表現するために使われるファイルフォーマットです。このフォーマットは、人間が読みやすいテキスト形式で、構造化されたデータを表現することができます。プログラマーはYAMLを使用することで、データの読み書きを容易に行うことができます。

## 使い方：
```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Yaml

main :: IO ()
main = do
  -- yamlファイルを読み込み
  file <- readFile "example.yml"

  -- パースされたYAMLを取得
  let result = decodeEither' file :: Either String Value

  case result of
    Left err -> putStrLn $ "エラーが発生しました: " ++ err
    Right val -> print (val :: Value)
```

出力例：
```
Object (fromList [("name",String "John"),("age",Number 25),("hobbies",Array [String "reading",String "coding"])])
```

## もっと深く掘り下げる：
- YAMLは1990年代に作成され、Pythonの作者であるGuido van Rossumによって最初に開発されました。
- 他のファイルフォーマットとしてJSONやXMLがありますが、YAMLは人間が読み書きしやすいため、プロジェクトの設定や構成ファイルとして使用されることが多いです。
- HaskellではData.Yamlライブラリが提供されており、YAMLファイルの読み書きを行うことができます。また、例外処理のためにEither型が使用されます。

## 関連情報：
- [YAML Official website](https://yaml.org/)
- [Haskell Data.Yaml documentation](https://hackage.haskell.org/package/yaml/docs/Data-Yaml.html)