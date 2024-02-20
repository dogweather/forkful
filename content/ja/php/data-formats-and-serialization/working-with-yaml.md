---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:29.736577-07:00
description: "YAML\u306F \"YAML Ain't Markup Language\"\u2026"
lastmod: 2024-02-19 22:05:01.411099
model: gpt-4-0125-preview
summary: "YAML\u306F \"YAML Ain't Markup Language\"\u2026"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
---

{{< edit_this_page >}}

## 何となく理由

YAMLは "YAML Ain't Markup Language" の略で、人が読みやすいデータ直列化形式であり、通常は設定ファイルに使われます。そのシンプルさと読みやすさのため、プログラマーは設定、パラメーター、さらには複雑なデータ構造までをも簡単に管理できる形式で保存するためにYAMLを利用することを選択します。

## どのように:

現行のバージョンのPHPでは、標準ライブラリの一部としてYAMLの解析をサポートしていません。PHPでYAMLを扱う最も簡単な方法は、Symfony YAMLコンポーネントまたは`yaml` PECL拡張を使用することです。

### Symfony YAMLコンポーネントを使用する

まずは、Composer経由でSymfony YAMLコンポーネントをインストールします：

```bash
composer require symfony/yaml
```

次に、以下のようにYAMLコンテンツを解析・ダンプします：

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// YAMLの解析
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// 配列からYAMLを作成
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = Yaml::dump($array);
echo $yaml;
```

解析時のサンプル出力：

```
Array
(
    [greet] => Hello, World!
    [framework] => Array
        (
            [name] => Symfony
            [language] => PHP
        )

)
```

ダンプ時のサンプル出力：

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### `yaml` PECL拡張を使用する

プロジェクトの要件が許される場合、または好みであれば、PECL拡張もYAMLを扱うための効率的な方法の一つです。まず、拡張機能がインストールされていることを確認します：

```bash
pecl install yaml
```

次に、`php.ini`設定で有効にします：

```ini
extension=yaml.so
```

ここで、YAMLの解析と発行を行います：

```php
<?php

// YAMLの解析
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// 配列からYAMLを作成
$array = [
    'greet' => 'Hello, YAML!',
    'framework' => [
        'name' => 'Symfony',
        'language' => 'PHP',
    ],
];

$yaml = yaml_emit($array);
echo $yaml;
```

出力はSymfonyコンポーネントのものと似ており、YAMLが人間が読める形式とPHPの配列構造との間の橋渡しとしての役割を示しており、より簡単な設定やデータ処理を促進します。
