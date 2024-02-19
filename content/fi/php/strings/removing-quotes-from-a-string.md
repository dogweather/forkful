---
aliases:
- /fi/php/removing-quotes-from-a-string/
date: 2024-01-26 03:40:53.425198-07:00
description: "Merkkijonosta lainausmerkkien poistaminen PHP:ss\xE4 tarkoittaa niiden\
  \ kiusallisten kaksinkertaisten (`\"`) tai yksinkertaisten (`'`) lainausmerkkien\u2026"
lastmod: 2024-02-18 23:09:07.698664
model: gpt-4-0125-preview
summary: "Merkkijonosta lainausmerkkien poistaminen PHP:ss\xE4 tarkoittaa niiden kiusallisten\
  \ kaksinkertaisten (`\"`) tai yksinkertaisten (`'`) lainausmerkkien\u2026"
title: Merkkijonosta lainausmerkkien poistaminen
---

{{< edit_this_page >}}

## Mikä & Miksi?
Merkkijonosta lainausmerkkien poistaminen PHP:ssä tarkoittaa niiden kiusallisten kaksinkertaisten (`"`) tai yksinkertaisten (`'`) lainausmerkkien poistamista, jotka voivat sotkea koodilogiikkasi tai tietokantakyselyt. Ohjelmoijat tekevät sen puhdistaakseen tai desinfioidakseen syötetietoja, varmistaen että merkkijonot ovat turvallisesti käytettävissä tai tallennettu.

## Kuinka:
Tässä on suoraviivainen esimerkki PHP:n sisäänrakennettuja funktioita käyttäen:

```php
$quotedString = "'Hei,' hän sanoi, \"Onpa hieno päivä!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Tulostaa: Hei, hän sanoi, Onpa hieno päivä!
```

Yksinkertaista, eikö? Tämä `str_replace()` -funktio ottaa taulukon merkkejä, jotka poistetaan merkkijonosta, mukaan lukien sekä yksinkertaiset että kaksinkertaiset lainausmerkit.

## Syväsukellus
PHP:n alkuaikoina kehittäjien piti olla erityisen varovaisia merkkijonoihin sisältyvien lainausmerkkien kanssa, etenkin syötettäessä tietoja tietokantaan. Huolimattomasti käsitellyt lainausmerkit saattoivat johtaa SQL-injektiohyökkäyksiin. Tästä syystä otettiin käyttöön taikamerkit (magic quotes), ominaisuus, joka automaattisesti lisäsi koodinpätkiä syötetietoihin. Se kuitenkin vanhentui ja poistettiin käytöstä, koska se kannusti huonoihin koodauskäytäntöihin ja turvallisuusongelmiin.

Nyt käytämme funktioita kuten `str_replace()` tai regexiä `preg_replace()`-funktion kanssa monimutkaisempien mallien varten. Tässä on regex-esimerkki:

```php
$quotedString = "'Hei,' hän sanoi, \"Onpa hieno päivä!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

JSON-tiedon yhteydessä saatat käyttää `json_encode()`-funktiota vaihtoehdoilla, kuten `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE`, välttääksesi ylimääräisiä kenoviivoja lainausmerkeissäsi.

Toteutusta suunnitellessa harkitse reunatapauksia. Entä jos merkkijonosi on tarkoitettu sisältämään tiettyjä lainausmerkkejä, kuten dialogia tarinassa tai tuumamittoja? Konteksti on tärkeä, joten räätälöi lainausmerkkiesi poisto datan tarkoitetun käytön mukaan.

## Katso myös
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: SQL-injektion estäminen](https://owasp.org/www-community/attacks/SQL_Injection)
