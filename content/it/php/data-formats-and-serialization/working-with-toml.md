---
date: 2024-01-26 04:24:37.474126-07:00
description: "TOML, abbreviazione di Tom's Obvious, Minimal Language, \xE8 un formato\
  \ di dati simile a JSON o YAML, ma pi\xF9 facile da leggere per gli esseri umani.\
  \ I\u2026"
lastmod: 2024-02-19 22:05:02.607588
model: gpt-4-0125-preview
summary: "TOML, abbreviazione di Tom's Obvious, Minimal Language, \xE8 un formato\
  \ di dati simile a JSON o YAML, ma pi\xF9 facile da leggere per gli esseri umani.\
  \ I\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cosa e Perché?
TOML, abbreviazione di Tom's Obvious, Minimal Language, è un formato di dati simile a JSON o YAML, ma più facile da leggere per gli esseri umani. I programmatori lo utilizzano per file di configurazione perché è diretto e si traduce bene in strutture dati.

## Come fare:
Prima di tutto, assicurati di avere installato una libreria parser di TOML, come `yosymfony/toml`. Vediamo come analizzare un file TOML:

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

Esempio di Output:

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
## Approfondimento
TOML è stato creato nel 2013 da Tom Preston-Werner, co-fondatore di GitHub, come alternativa più user-friendly ad XML e JSON per i file di configurazione. Sebbene JSON sia semplice per le macchine, la struttura del TOML facilita la lettura umana, senza la complessità di YAML.

Le alternative a TOML includono JSON, YAML e XML. Ognuno ha i suoi punti di forza e scenari di applicazione. JSON è onnipresente e indipendente dal linguaggio; YAML è più leggibile e supporta i commenti, mentre XML è esteso e ampiamente supportato.

Quando si implementa TOML in PHP, si guardano le librerie che analizzano il suo contenuto in array o oggetti PHP. `yosymfony/toml` è un parser PHP che aderisce alla specifica v0.4.0 del TOML. Per rimanere aggiornati, controllare sempre la disponibilità di parser più recenti o aggiornamenti che supportano la versione più attuale di TOML (v1.0.0 al momento del mio ultimo aggiornamento).

## Vedi anche
- Specifica TOML: <https://toml.io/>
- Parser TOML per PHP (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Confronto tra Formati di Dati (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- Gestore di pacchetti PHP (Composer): <https://getcomposer.org/>
