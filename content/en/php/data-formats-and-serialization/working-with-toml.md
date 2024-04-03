---
date: 2024-01-25 03:39:36.379349-07:00
description: 'How to: First, make sure you''ve got a TOML parser library installed,
  like `yosymfony/toml`. Let''s parse a TOML file.'
lastmod: '2024-03-13T22:45:00.188547-06:00'
model: gpt-4-1106-preview
summary: First, make sure you've got a TOML parser library installed, like `yosymfony/toml`.
title: Working with TOML
weight: 39
---

## How to:
First, make sure you've got a TOML parser library installed, like `yosymfony/toml`. Let's parse a TOML file:

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

Sample Output:

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

## Deep Dive
TOML came about in 2013, crafted by GitHub co-founder Tom Preston-Werner as a more user-friendly alternative to XML and JSON for config files. While JSON is simple for machines, TOML's structure makes it easy on human eyes, without the complexity of YAML.

Alternatives to TOML include JSON, YAML, and XML. Each has its strengths and application scenarios. JSON is ubiquitous and language-independent; YAML is more readable and supports comments, while XML is extensive and widely supported.

When implementing TOML in PHP, you're looking at libraries that parse its content into PHP arrays or objects. `yosymfony/toml` is a PHP parser that adheres to v0.4.0 of the TOML spec. To keep up with the latest, always check for newer parsers or updates that support the most current TOML version (v1.0.0 as of my last update).

## See Also
- TOML Specification: <https://toml.io/>
- TOML Parser for PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Comparing Data Formats (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP Package Manager (Composer): <https://getcomposer.org/>
