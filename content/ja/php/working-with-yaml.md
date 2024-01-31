---
title:                "YAMLを扱う"
date:                  2024-01-19
simple_title:         "YAMLを扱う"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？

YAMLは設定ファイルやデータの保存に使われる形式。PHPでYAMLを扱う理由は、その明瞭さと人間に優しい構造のため。設定やデータの読み書きを容易にする。

## How to:
やり方：

PHPでは`yaml_parse`と`yaml_emit`関数を使用してYAMLデータの読み込みと書き込みを行えます。次の例では、YAML文字列をPHPの配列に変換し、逆の操作も示します。

```PHP
<?php
// YAML文字列をPHP配列に変換
$yamlString = "
user: john_doe
email: john@example.com
enabled: true
roles:
  - admin
  - user
";
$array = yaml_parse($yamlString);
print_r($array);

// PHP配列をYAML文字列に変換
$arrayToYaml = [
    'user' => 'john_doe',
    'email' => 'john@example.com',
    'enabled' => true,
    'roles' => ['admin', 'user'],
];
$yaml = yaml_emit($arrayToYaml);
echo $yaml;
?>
```

出力例（配列変換）:
```
Array
(
    [user] => john_doe
    [email] => john@example.com
    [enabled] => 1
    [roles] => Array
        (
            [0] => admin
            [1] => user
        )
)
```

出力例（YAML変換）:
```
user: john_doe
email: john@example.com
enabled: true
roles:
  - admin
  - user
```

## Deep Dive
深掘り：

YAML（YAML Ain't Markup Language）は2001年に開始され、データを直感的に理解しやすい形で表現することを目的としています。JSONやXMLといった形式と比較されるが、YAMLは可読性を重視。PHPでは`yaml`拡張モジュールをインストールすることで機能を利用可能。インストールはPECLを通じて行われ、`pecl install yaml`コマンドで完了。

## See Also
関連情報：

- PHP公式ドキュメント内のYAML関数: [https://www.php.net/manual/en/ref.yaml.php](https://www.php.net/manual/en/ref.yaml.php)
- YAML公式サイト: [https://yaml.org](https://yaml.org)
- YAML仕様: [https://yaml.org/spec/1.2/spec.html](https://yaml.org/spec/1.2/spec.html)
