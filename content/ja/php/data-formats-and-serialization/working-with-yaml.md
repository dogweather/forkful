---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:29.736577-07:00
description: "\u3069\u306E\u3088\u3046\u306B: \u73FE\u884C\u306E\u30D0\u30FC\u30B8\
  \u30E7\u30F3\u306EPHP\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306E\u4E00\u90E8\u3068\u3057\u3066YAML\u306E\u89E3\u6790\u3092\u30B5\u30DD\u30FC\
  \u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u3002PHP\u3067YAML\u3092\u6271\u3046\u6700\
  \u3082\u7C21\u5358\u306A\u65B9\u6CD5\u306F\u3001Symfony YAML\u30B3\u30F3\u30DD\u30FC\
  \u30CD\u30F3\u30C8\u307E\u305F\u306F`yaml` PECL\u62E1\u5F35\u3092\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u3067\u3059\u3002"
lastmod: '2024-04-05T21:53:43.125484-06:00'
model: gpt-4-0125-preview
summary: "\u73FE\u884C\u306E\u30D0\u30FC\u30B8\u30E7\u30F3\u306EPHP\u3067\u306F\u3001\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u4E00\u90E8\u3068\u3057\u3066YAML\u306E\
  \u89E3\u6790\u3092\u30B5\u30DD\u30FC\u30C8\u3057\u3066\u3044\u307E\u305B\u3093\u3002\
  PHP\u3067YAML\u3092\u6271\u3046\u6700\u3082\u7C21\u5358\u306A\u65B9\u6CD5\u306F\u3001\
  Symfony YAML\u30B3\u30F3\u30DD\u30FC\u30CD\u30F3\u30C8\u307E\u305F\u306F`yaml` PECL\u62E1\
  \u5F35\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3059\u3002"
title: "YAML \u3092\u64CD\u4F5C\u3059\u308B"
weight: 41
---

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
