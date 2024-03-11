---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:19.131684-07:00
description: "YAML\uFF0C\u5373\u201CYAML Ain't Markup Language\u201D\uFF08YAML\u4E0D\
  \u662F\u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\
  \u7684\u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u5E38\u7528\u4E8E\u914D\u7F6E\
  \u6587\u4EF6\u3002\u7A0B\u5E8F\u5458\u9009\u62E9\u4F7F\u7528YAML\u662F\u56E0\u4E3A\
  \u5B83\u7B80\u5355\u6613\u8BFB\uFF0C\u975E\u5E38\u9002\u5408\u4EE5\u6613\u4E8E\u7BA1\
  \u7406\u7684\u5F62\u5F0F\u5B58\u50A8\u8BBE\u7F6E\u3001\u53C2\u6570\u4E43\u81F3\u590D\
  \u6742\u7684\u6570\u636E\u7ED3\u6784\u3002"
lastmod: '2024-03-11T00:14:21.678139-06:00'
model: gpt-4-0125-preview
summary: "YAML\uFF0C\u5373\u201CYAML Ain't Markup Language\u201D\uFF08YAML\u4E0D\u662F\
  \u6807\u8BB0\u8BED\u8A00\uFF09\uFF0C\u662F\u4E00\u79CD\u4EBA\u7C7B\u53EF\u8BFB\u7684\
  \u6570\u636E\u5E8F\u5217\u5316\u683C\u5F0F\uFF0C\u5E38\u7528\u4E8E\u914D\u7F6E\u6587\
  \u4EF6\u3002\u7A0B\u5E8F\u5458\u9009\u62E9\u4F7F\u7528YAML\u662F\u56E0\u4E3A\u5B83\
  \u7B80\u5355\u6613\u8BFB\uFF0C\u975E\u5E38\u9002\u5408\u4EE5\u6613\u4E8E\u7BA1\u7406\
  \u7684\u5F62\u5F0F\u5B58\u50A8\u8BBE\u7F6E\u3001\u53C2\u6570\u4E43\u81F3\u590D\u6742\
  \u7684\u6570\u636E\u7ED3\u6784\u3002"
title: "\u4F7F\u7528YAML\u5DE5\u4F5C"
---

{{< edit_this_page >}}

## 什么 & 为什么？

YAML，即“YAML Ain't Markup Language”（YAML不是标记语言），是一种人类可读的数据序列化格式，常用于配置文件。程序员选择使用YAML是因为它简单易读，非常适合以易于管理的形式存储设置、参数乃至复杂的数据结构。

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
