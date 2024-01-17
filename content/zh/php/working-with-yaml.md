---
title:                "使用yaml进行编程"
html_title:           "PHP: 使用yaml进行编程"
simple_title:         "使用yaml进行编程"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么是YAML以及为什么程序员需要使用它?
YAML是一种轻量级的数据交换格式，它使用可读性较高的键值对结构，易于人类阅读和手动编辑。程序员常常使用YAML来存储和传输配置信息和其他结构化数据。

## 如何使用YAML:
```PHP
// 通过YAML扩展加载
extension_loaded( 'yaml' );

// 将PHP数组转换为YAML格式
$data = [
  'name' => 'John',
  'age' => 25,
  'hobbies' => [
    'reading',
    'hiking',
    'cooking'
  ]
];
$yaml = yaml_emit( $data );

// 输出样例:
name: John
age: 25
hobbies:
  - reading
  - hiking
  - cooking
```

## 深入了解:
YAML最初由Perl编程语言的作者开发，在2001年发布第一个版本。它的设计目标是比其他数据交换格式更轻量且易于阅读。除了PHP，YAML也被其他编程语言如Python、Ruby等广泛使用。除了使用YAML扩展，也可以使用第三方库如symfony/yaml来处理YAML数据。

## 参考资源:
- [YAML官方网站](https://yaml.org/)
- [symfony/yaml GitHub页面](https://github.com/symfony/yaml)
- [YAML扩展文档](https://www.php.net/manual/zh/book.yaml.php)