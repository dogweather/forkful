---
date: 2024-01-26 04:25:17.645716-07:00
description: "TOML, lyhenteen\xE4 Tom's Obvious, Minimal Language, on datamuoto, joka\
  \ muistuttaa JSONia tai YAMLia, mutta on ihmisten luettavissa helpompi. Ohjelmoijat\u2026"
lastmod: '2024-02-25T18:49:53.591147-07:00'
model: gpt-4-0125-preview
summary: "TOML, lyhenteen\xE4 Tom's Obvious, Minimal Language, on datamuoto, joka\
  \ muistuttaa JSONia tai YAMLia, mutta on ihmisten luettavissa helpompi. Ohjelmoijat\u2026"
title: "Ty\xF6skentely TOML:n kanssa"
---

{{< edit_this_page >}}

## Mikä & Miksi?
TOML, lyhenteenä Tom's Obvious, Minimal Language, on datamuoto, joka muistuttaa JSONia tai YAMLia, mutta on ihmisten luettavissa helpompi. Ohjelmoijat käyttävät sitä kokoonpanotiedostoihin, koska se on suoraviivainen ja kääntyy hyvin datarakenteiksi.

## Kuinka:
Ensin varmista, että sinulla on asennettuna TOML-jäsentäjäkirjasto, kuten `yosymfony/toml`. Aloitetaan TOML-tiedoston jäsentäminen:

```php
composer require yosymfony/toml

<?php
require 'vendor/autoload.php';

use Yosymfony\Toml\Toml;

$tomlMerkkijono = <<<TOML
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
TOML;

$taulukko = Toml::Parse($tomlMerkkijono);

print_r($taulukko);
```

Esimerkkituloste:

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
## Syväluotaus
TOML syntyi vuonna 2013, jonka loi GitHubin perustajajäsen Tom Preston-Werner tarjoamaan käyttäjäystävällisemmän vaihtoehdon XML:lle ja JSONille kokoonpanotiedostoissa. Vaikka JSON on yksinkertainen koneille, TOML:n rakenne tekee siitä helpon ihmisen silmille, ilman YAML:n monimutkaisuutta.

Vaihtoehtoja TOML:lle ovat JSON, YAML ja XML. Jokaisella on vahvuutensa ja käyttöskenaarionsa. JSON on kaikkialla läsnä oleva ja kieliriippumaton; YAML on luettavampi ja tukee kommentteja, kun taas XML on laajalti tuettu ja kattava.

TOML:n toteuttaminen PHP:ssä tarkoittaa kirjastoja, jotka jäsentävät sen sisällön PHP-taulukoiksi tai -objekteiksi. `yosymfony/toml` on PHP-jäsennin, joka noudattaa TOML-spesifikaation v0.4.0 versiota. Pysyäksesi ajan tasalla, tarkista aina uudemmat jäsentäjät tai päivitykset, jotka tukevat uusinta TOML-versiota (v1.0.0 viimeisimmän päivitykseni mukaan).

## Katso Myös
- TOML-spesifikaatio: <https://toml.io/>
- TOML-jäsennin PHP:lle (`yosymfony/toml`): <https://github.com/yosymfony/toml>
- Dataformaattien vertailu (XML, JSON, YAML, TOML): <https://www.loginradius.com/blog/engineering/comparing-data-interchange-formats/>
- PHP-pakettienhallinta (Composer): <https://getcomposer.org/>
