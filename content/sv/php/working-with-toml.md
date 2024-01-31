---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:24:34.205829-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/php/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
TOML, kort för Toms Uppenbara, Minimala Språk, är ett dataformat liknande JSON eller YAML, men lättare att läsa för människor. Programmerare använder det för konfigurationsfiler eftersom det är enkelt och översätter väl till datastrukturer.

## Hur man gör:
Först, se till att du har ett TOML-parserbibliotek installerat, som `yosymfony/toml`. Låt oss tolka en TOML-fil:

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

Exempel på utdata:

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
## Djupdykning
TOML dök upp 2013, skapat av GitHub:s medgrundare Tom Preston-Werner som ett mer användarvänligt alternativ till XML och JSON för konfigurationsfiler. Medan JSON är enkelt för maskiner, gör TOML:s struktur det lättförståeligt för människor, utan komplexiteten hos YAML.

Alternativ till TOML inkluderar JSON, YAML och XML. Varje format har sina styrkor och applikationsscenarier. JSON är allestädes närvarande och språkoberoende; YAML är mer läsligt och stöder kommentarer, medan XML är omfattande och brett stött.

När du implementerar TOML i PHP, tittar du på bibliotek som tolkar dess innehåll till PHP-arrayer eller objekt. `yosymfony/toml` är en PHP-tolkare som följer v0.4.0 av TOML-specifikationen. För att hålla dig uppdaterad, kontrollera alltid efter nyare parsrar eller uppdateringar som stöder den senaste TOML-versionen (v1.0.0 vid min senaste uppdatering).

## Se också
- TOML-specifikation: <https://toml.io/>
- TOML-parser för PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Jämförelse av dataformat (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP-pakethanterare (Composer): <https://getcomposer.org/>
