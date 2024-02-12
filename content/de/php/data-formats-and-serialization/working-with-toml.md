---
title:                "Arbeiten mit TOML"
aliases: - /de/php/working-with-toml.md
date:                  2024-01-26T04:24:30.415893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML, die Abkürzung für Tom's Obvious, Minimal Language, ist ein Datenformat ähnlich zu JSON oder YAML, jedoch leichter für Menschen zu lesen. Programmierer verwenden es für Konfigurationsdateien, da es unkompliziert ist und sich gut in Datenstrukturen übersetzen lässt.

## Wie geht das:
Zuerst stellen Sie sicher, dass Sie eine TOML-Parser-Bibliothek installiert haben, wie `yosymfony/toml`. Lassen Sie uns eine TOML-Datei parsen:

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

Beispielausgabe:

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
## Tiefere Einblicke
TOML entstand 2013, kreiert vom Mitbegründer von GitHub, Tom Preston-Werner, als eine benutzerfreundlichere Alternative zu XML und JSON für Konfigurationsdateien. Während JSON einfach für Maschinen ist, macht die Struktur von TOML es leicht auf die menschlichen Augen, ohne die Komplexität von YAML.

Alternativen zu TOML sind JSON, YAML und XML. Jedes hat seine Stärken und Anwendungsszenarien. JSON ist allgegenwärtig und sprachunabhängig; YAML ist lesbarer und unterstützt Kommentare, während XML umfangreich und weit verbreitet ist.

Wenn Sie TOML in PHP implementieren, schauen Sie sich Bibliotheken an, die dessen Inhalte in PHP-Arrays oder -Objekte parsen. `yosymfony/toml` ist ein PHP-Parser, der der TOML-Spezifikation v0.4.0 entspricht. Um auf dem Laufenden zu bleiben, überprüfen Sie immer nach neueren Parseern oder Updates, die die aktuellste TOML-Version unterstützen (v1.0.0 bei meinem letzten Update).

## Siehe auch
- TOML-Spezifikation: <https://toml.io/>
- TOML-Parser für PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Vergleich von Datenformaten (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP-Paketmanager (Composer): <https://getcomposer.org/>
