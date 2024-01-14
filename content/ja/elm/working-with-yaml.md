---
title:                "Elm: 「Yamlを使う」"
simple_title:         "「Yamlを使う」"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜYAMLを使うのか

YAMLは、人間にとって読みやすく書けるテキスト形式のデータフォーマットです。プログラミング言語の一つであるElmでは、YAMLを使ってデータを読み込んだり、書き出したりすることができます。また、YAMLはJsonやXMLよりもシンプルな構造を持っているため、データの管理や保存にも便利です。

## 使い方

まずは、YAMLを読み込むために必要なライブラリをインストールします。

```Elm
elm install elm-community/yaml
```

次に、コードの中でYAMLを使うためのパッケージをインポートします。

```Elm
import Yaml
```

YAMLを読み込んでJsonデータに変換するには、次のようにコードを書きます。

```Elm
yamlString = toString """
name: John
age: 25
"""
result = Yaml.decodeString yamlString
```

YAMLからJsonデータへの変換が成功した場合、`Result`型の値としてデータが返されます。

```Elm
case result of
  Ok jsonData -> -- 変換に成功した場合の処理を書く
  Err error -> -- エラーが発生した場合の処理を書く
```

また、JsonデータからYAMLに変換することもできます。

```Elm
jsonData = Json.Encode.object [ ( "name", Json.Encode.string "John" ), ( "age", Json.Encode.int 25 ) ]
yamlString = Yaml.encode 0 jsonData
```

`encode`関数の第一引数は、YAMLのインデント数を指定するものです。ここでは0を指定しています。

## ディープダイブ

YAMLは、プログラムの設定ファイルやデータのシリアライズにも使うことができます。また、YAMLはコメントを記述することも可能です。例えば、次のように書くことでデータに対する説明をコメントとして追加することができます。

```Elm
--- 追加したコメントはこのように書きます
title: Elmプログラミング入門
author: John
```

YAMLは、複数行にわたる文字列の表記方法もあります。これを使うことで、長い文章や複数の行にわたるデータを簡単に記述することができます。

```Elm
longText: |-
  ここには長い文章や
  複数の行にわたるデータを
  簡単に記述することができます。
```

さらにYAMLには、リストやマップといったデータ構造を表現するための機能もあります。詳細は公式ドキュメントを参照してください。

## 関連リンク

- [Elm Community/YAML](https://github.com/elm-community/yaml)
- [YAML 公式ドキュメント](https://yaml.org/)
- [YAMLの基本構文](https://www.geeksforgeeks.org/dummies-guide-to-yaml-basics/) (英語)