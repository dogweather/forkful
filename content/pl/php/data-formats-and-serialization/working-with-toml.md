---
date: 2024-01-26 04:24:53.483613-07:00
description: "Jak to zrobi\u0107: Najpierw upewnij si\u0119, \u017Ce masz zainstalowan\u0105\
  \ bibliotek\u0119 parsera TOML, tak\u0105 jak `yosymfony/toml`. Spr\xF3bujmy przet\u0142\
  umaczy\u0107 plik TOML."
lastmod: '2024-03-13T22:44:35.521005-06:00'
model: gpt-4-0125-preview
summary: "Najpierw upewnij si\u0119, \u017Ce masz zainstalowan\u0105 bibliotek\u0119\
  \ parsera TOML, tak\u0105 jak `yosymfony/toml`."
title: Praca z TOML
weight: 39
---

## Jak to zrobić:
Najpierw upewnij się, że masz zainstalowaną bibliotekę parsera TOML, taką jak `yosymfony/toml`. Spróbujmy przetłumaczyć plik TOML:

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

Przykładowe wyjście:

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

## Szczegółowa analiza
TOML powstał w 2013 roku, stworzony przez współzałożyciela GitHuba, Toma Preston-Wernera, jako bardziej przyjazna alternatywa dla XML i JSON w plikach konfiguracyjnych. Chociaż JSON jest prosty dla maszyn, struktura TOML ułatwia jego odczyt ludziom, bez skomplikowanych elementów YAML.

Alternatywami dla TOML są JSON, YAML i XML. Każdy z nich ma swoje mocne strony i scenariusze zastosowań. JSON jest wszechobecny i niezależny od języka; YAML jest bardziej czytelny i obsługuje komentarze, podczas gdy XML jest obszerny i szeroko wspierany.

Implementując TOML w PHP, szukasz bibliotek, które parsują jego zawartość na tablice lub obiekty PHP. `yosymfony/toml` to parser PHP, który jest zgodny z wersją v0.4.0 specyfikacji TOML. Aby być na bieżąco z najnowszymi wersjami, zawsze sprawdzaj nowsze parsery lub aktualizacje wspierające najnowszą wersję TOML (v1.0.0 zgodnie z moją ostatnią aktualizacją).

## Zobacz także
- Specyfikacja TOML: <https://toml.io/>
- Parser TOML dla PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Porównanie formatów danych (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Menedżer pakietów PHP (Composer): <https://getcomposer.org/>
