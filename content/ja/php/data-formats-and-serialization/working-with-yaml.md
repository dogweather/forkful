---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:29.736577-07:00
description: "YAML\u306F \"YAML Ain't Markup Language\"\u2026"
lastmod: '2024-03-13T22:44:42.274809-06:00'
model: gpt-4-0125-preview
summary: "YAML\u306F \"YAML Ain't Markup Language\" \u306E\u7565\u3067\u3001\u4EBA\
  \u304C\u8AAD\u307F\u3084\u3059\u3044\u30C7\u30FC\u30BF\u76F4\u5217\u5316\u5F62\u5F0F\
  \u3067\u3042\u308A\u3001\u901A\u5E38\u306F\u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u306B\
  \u4F7F\u308F\u308C\u307E\u3059\u3002\u305D\u306E\u30B7\u30F3\u30D7\u30EB\u3055\u3068\
  \u8AAD\u307F\u3084\u3059\u3055\u306E\u305F\u3081\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u8A2D\u5B9A\u3001\u30D1\u30E9\u30E1\u30FC\u30BF\u30FC\u3001\u3055\u3089\
  \u306B\u306F\u8907\u96D1\u306A\u30C7\u30FC\u30BF\u69CB\u9020\u307E\u3067\u3092\u3082\
  \u7C21\u5358\u306B\u7BA1\u7406\u3067\u304D\u308B\u5F62\u5F0F\u3067\u4FDD\u5B58\u3059\
  \u308B\u305F\u3081\u306BYAML\u3092\u5229\u7528\u3059\u308B\u3053\u3068\u3092\u9078\
  \u629E\u3057\u307E\u3059\u3002."
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
