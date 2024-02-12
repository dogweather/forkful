---
title:                "使用TOML"
aliases:
- zh/php/working-with-toml.md
date:                  2024-01-26T04:24:48.765412-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-toml.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
TOML，即Tom's Obvious, Minimal Language（汤姆的明显的、最小的语言）的缩写，是一种类似于JSON或YAML的数据格式，但对人类来说更易于阅读。程序员之所以使用它来作为配置文件，是因为它直截了当且能够很好地转换为数据结构。

## 如何操作:
首先，确认你已经安装了TOML解析库，例如`yosymfony/toml`。让我们来解析一个TOML文件：

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

示例输出：

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [ports] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [connection_max] => 5000
            [enabled] => 1
        )

)
```
## 深入了解
TOML 在 2013 年由 GitHub 共同创始人汤姆·普雷斯顿-沃纳开发，作为一种相比 XML 和 JSON 更加用户友好的配置文件替代方案。尽管 JSON 对机器来说很简单，但 TOML 的结构对人类眼睛来说易于阅读，而没有 YAML 的复杂性。

TOML 的替代方案包括 JSON、YAML 和 XML。每种都有其优势和应用场景。JSON 无处不在且与语言无关；YAML 更具可读性且支持评论；而 XML 则广泛支持且拥有广泛的支持。

在 PHP 中实现 TOML，你会查找将其内容解析成 PHP 数组或对象的库。`yosymfony/toml` 是一个遵循 TOML 规范 v0.4.0 的 PHP 解析器。为了跟上最新进展，始终检查是否有支持最新 TOML 版本（我最后更新时为 v1.0.0）的新解析器或更新。

## 另见
- TOML 规范：<https://toml.io/>
- PHP 的 TOML 解析器（`yosymfony/toml`）：<https://github.com/yosymfony/toml>
- 数据格式比较（XML、JSON、YAML、TOML）：<https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP 包管理器（Composer）：<https://getcomposer.org/>
