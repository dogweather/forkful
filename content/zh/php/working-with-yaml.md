---
title:                "处理 YAML 文件"
html_title:           "Bash: 处理 YAML 文件"
simple_title:         "处理 YAML 文件"
programming_language: "PHP"
category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)
YAML 是 "YAML Ain't Markup Language" 的缩写，一种易于阅读的数据序列化标准。PHP程序员使用YAML是因为它清晰、直观，适合配置文件、数据交换。

## How to: (怎么做：)
PHP 使用 `yaml` 扩展来解析和生成 YAML。要操作 YAML，你需要安装这个扩展。

```PHP
<?php
// 安装扩展：pecl install yaml

// 解析 YAML 字符串
$yamlString = "
shop:
  book:
    - title: 'Learning PHP'
      price: 10
    - title: 'Mastering YAML'
      price: 15
";

$array = yaml_parse($yamlString);

// 输出数组
print_r($array);

// 生成 YAML 字符串
$arrayToYaml = [
    'store' => [
        'fruit' => [
            ['name' => 'apple', 'color' => 'red'],
            ['name' => 'banana', 'color' => 'yellow'],
        ],
    ],
];

$yamlOutput = yaml_emit($arrayToYaml);
echo $yamlOutput;
```

样例输出：
```
Array
(
    [shop] => Array
        (
            [book] => Array
                (
                    [0] => Array
                        (
                            [title] => Learning PHP
                            [price] => 10
                        )

                    [1] => Array
                        (
                            [title] => Mastering YAML
                            [price] => 15
                        )
                )
        )
)

store:
  fruit:
    - name: apple
      color: red
    - name: banana
      color: yellow
```

## Deep Dive (深入探索)
YAML 从 2001 年起就有了，用于取代复杂的 XML。相比 JSON，YAML 更注重人类可读性，但JSON更紧凑、解析更快。在PHP中，`yaml_parse()` 和 `yaml_emit()`是主要函数，内部解析细节依赖LibYAML库，这保证了速度和兼容性。

## See Also (另请参阅)
- PHP 官方文档关于 YAML 扩展: [https://www.php.net/manual/en/book.yaml.php](https://www.php.net/manual/en/book.yaml.php)
- YAML 官方网站，了解YAML标准: [https://yaml.org/](https://yaml.org/)
- LibYAML 项目页面，了解底层库: [http://pyyaml.org/wiki/LibYAML](http://pyyaml.org/wiki/LibYAML)