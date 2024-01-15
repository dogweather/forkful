---
title:                "YAMLでの作業"
html_title:           "PHP: YAMLでの作業"
simple_title:         "YAMLでの作業"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## なぜ
YAMLを使ったプログラミングに取り組む理由は多岐にわたりますが、一つの理由としては、YAMLが人間にとって読み書きしやすいフォーマットであるということが挙げられます。

## 使い方
まず、PHPでYAMLを読み込む方法を紹介します。以下のコードを使い、`example.yaml`ファイルを読み込んでみましょう。

```PHP
<?php
$yaml = file_get_contents('example.yaml');
$data = yaml_parse($yaml);
print_r($data);
```

そして、`example.yaml`ファイルには以下のようなデータが書かれているとします。

```YAML
name: John Smith
age: 25
favorite_foods:
  - sushi
  - pizza
```

上記のコードを実行すると、以下のような結果が表示されます。

```PHP
Array
(
    [name] => John Smith
    [age] => 25
    [favorite_foods] => Array
        (
            [0] => sushi
            [1] => pizza
        )
)
```

YAMLは、複数の言語で扱うことができるフォーマットです。そのため、上記のコードは他の言語でも同様に動作します。また、YAMLを使うことで、複雑なデータをよりシンプルに表現することができます。

## より詳しく
YAMLは、それ自体がプログラミング言語ではありませんが、データの構造を表現するための非常に便利なツールです。YAMLは、タブや空白を使って階層を表現することができ、データをより見やすく記述できるため、コーディングの効率を上げることができます。

## 参考リンク
- [YAML公式サイト](https://yaml.org/)
- [PHPマニュアル - yaml_parse](https://www.php.net/manual/en/function.yaml-parse.php)
- [YAMLを使ってみよう](https://www.oreilly.co.jp/books/9784873113367/)