---
aliases:
- /zh/php/working-with-toml/
date: 2024-01-26 04:24:48.765412-07:00
description: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\
  \u660E\u663E\u7684\u3001\u6700\u5C0F\u7684\u8BED\u8A00\uFF09\u7684\u7F29\u5199\uFF0C\
  \u662F\u4E00\u79CD\u7C7B\u4F3C\u4E8EJSON\u6216YAML\u7684\u6570\u636E\u683C\u5F0F\
  \uFF0C\u4F46\u5BF9\u4EBA\u7C7B\u6765\u8BF4\u66F4\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\
  \u5E8F\u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\u6765\u4F5C\u4E3A\u914D\u7F6E\u6587\
  \u4EF6\uFF0C\u662F\u56E0\u4E3A\u5B83\u76F4\u622A\u4E86\u5F53\u4E14\u80FD\u591F\u5F88\
  \u597D\u5730\u8F6C\u6362\u4E3A\u6570\u636E\u7ED3\u6784\u3002"
lastmod: 2024-02-18 23:08:59.237255
model: gpt-4-0125-preview
summary: "TOML\uFF0C\u5373Tom's Obvious, Minimal Language\uFF08\u6C64\u59C6\u7684\u660E\
  \u663E\u7684\u3001\u6700\u5C0F\u7684\u8BED\u8A00\uFF09\u7684\u7F29\u5199\uFF0C\u662F\
  \u4E00\u79CD\u7C7B\u4F3C\u4E8EJSON\u6216YAML\u7684\u6570\u636E\u683C\u5F0F\uFF0C\
  \u4F46\u5BF9\u4EBA\u7C7B\u6765\u8BF4\u66F4\u6613\u4E8E\u9605\u8BFB\u3002\u7A0B\u5E8F\
  \u5458\u4E4B\u6240\u4EE5\u4F7F\u7528\u5B83\u6765\u4F5C\u4E3A\u914D\u7F6E\u6587\u4EF6\
  \uFF0C\u662F\u56E0\u4E3A\u5B83\u76F4\u622A\u4E86\u5F53\u4E14\u80FD\u591F\u5F88\u597D\
  \u5730\u8F6C\u6362\u4E3A\u6570\u636E\u7ED3\u6784\u3002"
title: "\u4F7F\u7528TOML"
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
