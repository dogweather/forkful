---
title:                "「YAMLでの作業」"
html_title:           "C++: 「YAMLでの作業」"
simple_title:         "「YAMLでの作業」"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ

プログラミング言語C++でYAMLを使用する理由は、構造化されたデータのストレージや転送に最適なフォーマットとして知られているためです。また、YAMLは人間にとっても読みやすい形式であり、データを簡単に修正できるため、開発作業の効率性を高めることができます。

## 使い方

YAMLをC++で使うための簡単なコード例を示します。

```C++
#include <iostream>
#include <yaml-cpp/yaml.h>

int main()
{
  // YAMLドキュメントの作成
  YAML::Node doc;
  doc["name"] = "Miku";
  doc["age"] = 18;
  doc["hobbies"] = YAML::Load("[Singing, Dancing, Programming]");

  // YAMLドキュメントの表示
  std::cout << doc << std::endl;

  // YAMLファイルに保存
  std::ofstream fout("profile.yml");
  fout << doc;

  return 0;
}
```

上記のコードでは、まず`yaml-cpp`ライブラリをインポートし、YAMLドキュメントを作成します。その後、ドキュメントにデータを追加し、表示したりファイルに保存したりできます。上記のコードを実行すると、以下のような出力が得られます。

```yaml
name: Miku
age: 18
hobbies: [Singing, Dancing, Programming]
```

### ディープダイブ

YAMLを扱う上でよく使われる2つの基本的なデータ型は、マップ（`map`）とシーケンス（`seq`）です。マップは`key: value`という形式でデータを保持し、シーケンスは順番にデータを並べて保持します。また、YAMLでは`#`を使ってコメントを記述することができます。

例えば、以下のようなYAMLドキュメントがあったとします。

```yaml
# 家族の情報
family:
  # 父親の情報
  father:
    name: Taro
    age: 45
  # 母親の情報
  mother:
    name: Hanako
    age: 42
  # 子どもの情報
  children:
    - name: Miku
      age: 18
    - name: Yumi
      age: 13
```

このYAMLドキュメントでは、`family`というマップの中に`father`、`mother`、`children`というキーがあり、それぞれに対応するデータがマップまたはシーケンスで保持されています。データの参照はドット演算子`.`を使って行うことができます。例えば`family.father.name`とすると、父親の名前である`Taro`が取得できます。

YAMLについてさらに詳しく知りたい方は、公式のドキュメントを参考にすると良いでしょう。

## See Also

- [YAML公式ドキュメント](https://yaml.org/)
- [yaml-cppライブラリのGitHubページ](https://github.com/jbeder/yaml-cpp)
- [YAMLとは？ - Qiita](https://qiita.com/yoskeoka/items/9eb57c9c067cfb84659d)