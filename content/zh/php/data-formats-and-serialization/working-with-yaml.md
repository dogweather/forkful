---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:19.131684-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u622A\u81F3\u5F53\u524D\u7248\u672C\uFF0C\
  PHP\u5C1A\u4E0D\u652F\u6301\u5728\u5176\u6807\u51C6\u5E93\u4E2D\u89E3\u6790YAML\u3002\
  \u5728PHP\u4E2D\u4F7F\u7528YAML\u6700\u76F4\u63A5\u7684\u65B9\u5F0F\u662F\u901A\u8FC7\
  \u4F7F\u7528Symfony YAML\u7EC4\u4EF6\u6216\u8005`yaml` PECL\u6269\u5C55\u3002"
lastmod: '2024-04-05T21:53:48.193397-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
weight: 41
---

## 如何操作：
截至当前版本，PHP尚不支持在其标准库中解析YAML。在PHP中使用YAML最直接的方式是通过使用Symfony YAML组件或者`yaml` PECL扩展。

### 使用Symfony YAML组件
首先，通过Composer安装Symfony YAML组件：

```bash
composer require symfony/yaml
```

然后，您可以按如下方式解析和生成YAML内容：

```php
<?php
require_once __DIR__.'/vendor/autoload.php';

use Symfony\Component\Yaml\Yaml;

// 解析YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = Yaml::parse($yamlString);
print_r($array);

// 从数组创建YAML
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

解析时的示例输出：

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

生成时的示例输出：

```
greet: Hello, YAML!
framework:
    name: Symfony
    language: PHP
```

### 使用`yaml` PECL扩展
如果您愿意，或者如果您的项目要求允许，PECL扩展可以是另一种有效的方法来处理YAML。首先，确保安装了扩展：

```bash
pecl install yaml
```

然后，在您的`php.ini`配置中启用它：

```ini
extension=yaml.so
```

以下是如何解析和发出YAML：

```php
<?php

// 解析YAML
$yamlString = <<<YAML
greet: Hello, World!
framework:
  name: Symfony
  language: PHP
YAML;

$array = yaml_parse($yamlString);
print_r($array);

// 从数组创建YAML
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

输出将与Symfony组件的类似，展示了YAML作为人类可读格式与PHP数组结构之间桥梁的角色，方便了配置和数据处理。
