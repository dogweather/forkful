---
title:                "Haskell: 「ヤムルとの作業」"
simple_title:         "「ヤムルとの作業」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ

YAMLを使ったプログラミングに取り組む理由は様々あります。例えば、データの可読性や変更の容易さなどのメリットがあげられます。

## 方法

下の例では、HaskellでYAMLファイルを読み込み、データを表示する方法を紹介します。

```Haskell
import Data.Yaml

main :: IO ()
main = do
  -- YAMLファイルを読み込む
  yamlData <- decodeFileThrow "data.yaml" :: IO Value
  -- データを表示する
  print yamlData
```

上記の例では、Data.Yamlライブラリを使ってYAMLファイルを読み込み、データを表示することができます。IOモナドを使うことで、ファイルの読み込みやデータの表示を行うことができます。

```yaml
# data.yaml
- name: John
  age: 30
  occupation: Programmer
- name: Jane
  age: 25
  occupation: Designer
```

上記のようなYAMLファイルを読み込んだ場合、以下のような出力が得られます。

```
Array [Object (fromList [("name",String "John"),("age",Number 30.0),("occupation",String "Programmer")]),Object (fromList [("name",String "Jane"),("age",Number 25.0),("occupation",String "Designer")])]
```

## 深い掘り下げ

YAMLとは、Human Friendly Markup Language (ヒューマンフレンドリーなマークアップ言語) の略であり、データの表現や記述を行うためのフォーマットです。YAMLはPythonやRubyなどのプログラミング言語からもサポートされており、多くのプロジェクトで使用されています。

YAMLを扱う上で重要な概念の1つに「インデント」があります。インデントはスペース4つごとに行われ、データの階層を表すために使用されます。また、コロン（:）やハイフン（-）などの記号も重要な役割を果たします。

さらに、YAMLではリストや辞書などのデータ構造を柔軟に表現することができます。コードの見やすさを重視する場合や、構造化されたデータを扱う必要がある場合にはYAMLが役立つでしょう。

## この記事の他のリンク

[YAML公式サイト](https://yaml.org/)

[Haskell Data.Yamlライブラリのドキュメント](https://hackage.haskell.org/package/yaml-0.11.5.1/docs/Data-Yaml.html)

[YAML vs JSONの比較](https://stackoverflow.com/questions/1726802/yaml-or-json-which-is-better-faster-for-configuration-data)