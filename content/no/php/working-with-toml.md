---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:24:37.477933-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/php/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML, som står for Tom's Obvious, Minimal Language, er et dataformat liknende JSON eller YAML, men enklere å lese for mennesker. Programmerere bruker det til konfigurasjonsfiler fordi det er ukomplisert og oversetter godt til datastrukturer.

## Hvordan:
Først, sørg for at du har installert et TOML parser bibliotek, som `yosymfony/toml`. La oss parse en TOML-fil:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlString = <<<TOML
[database]
server = "192.168.1.1"
porter = [ 8001, 8001, 8002 ]
maks_forbindelse = 5000
aktivert = sant
TOML;

$array = Toml::Parse($tomlString);

print_r($array);
```

Eksempel på utskrift:

```
Array
(
    [database] => Array
        (
            [server] => 192.168.1.1
            [porter] => Array
                (
                    [0] => 8001
                    [1] => 8001
                    [2] => 8002
                )

            [maks_forbindelse] => 5000
            [aktivert] => 1
        )

)
```
## Dypdykk
TOML kom omkring i 2013, tilvirket av GitHub-medgrunnlegger Tom Preston-Werner som et mer brukervennlig alternativ til XML og JSON for konfigurasjonsfiler. Mens JSON er enkelt for maskiner, gjør TOMLs struktur det lett på menneskelige øyne, uten kompleksiteten til YAML.

Alternativer til TOML inkluderer JSON, YAML, og XML. Hver har sine styrker og anvendelsesscenarier. JSON er allestedsnærværende og språkuavhengig; YAML er mer lesbart og støtter kommentarer, mens XML er omfattende og bredt støttet.

Når du implementerer TOML i PHP, ser du etter biblioteker som parser innholdet til PHP-arrays eller objekter. `yosymfony/toml` er en PHP-parser som overholder v0.4.0 av TOML-spesifikasjonen. For å holde deg oppdatert, sjekk alltid for nyere parsers eller oppdateringer som støtter den nyeste TOML-versjonen (v1.0.0 per min siste oppdatering).

## Se også
- TOML Spesifikasjon: <https://toml.io/>
- TOML Parser for PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Sammenligning av Dataformater (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP Pakkebehandler (Composer): <https://getcomposer.org/>
