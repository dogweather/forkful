---
title:                "Controleren of een directory bestaat"
date:                  2024-01-28T21:56:14.606191-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een directory bestaat laat je de aanwezigheid van een map in het bestandssysteem bevestigen voordat je probeert deze te gebruiken. Programmeurs doen dit om fouten te vermijden bij het lezen, schrijven of navigeren door mappen.

## Hoe te:

In PHP controleert `is_dir()` of een directory bestaat:

```PHP
$directory = "/pad/naar/dir";

if (is_dir($directory)) {
    echo "De directory bestaat.";
} else {
    echo "De directory bestaat niet.";
}
```

Voorbeelduitvoer:
```
De directory bestaat.
```
Of, als de directory inderdaad niet bestaat:
```
De directory bestaat niet.
```

Om fouten te onderdrukken en een gedetailleerdere controle te gebruiken, combineer je `is_dir()` met de functie `file_exists()`:

```PHP
$directory = "/pad/naar/dir";

if (file_exists($directory) && is_dir($directory)) {
    echo "De directory bestaat en is een map.";
} else {
    echo "De directory bestaat niet of is een bestand.";
}
```

## Diepere duik

`is_dir()` bestaat in PHP sinds versie 4.0.0, wat het mogelijk maakt om voorafgaand aan operaties die kunnen falen of fouten kunnen opleveren, te controleren op het bestaan ​​van mappen. Niet te verwarren met `file_exists()`, wat zowel voor bestanden als mappen controleert, `is_dir()` is specifiek voor mappen.

Voordat deze ingebouwde functies bestonden, konden programmeurs `opendir()` hebben gebruikt en gecontroleerd of er een false retourwaarde was om niet-bestaan ​​af te leiden. Dit was minder efficiënt en foutgevoeliger.

Onder de motorkap voert `is_dir()` een syscall uit naar het onderliggende bestandssysteem, wat kostbaarder kan zijn in termen van I/O-bewerkingen, vooral voor externe of virtuele bestandssystemen. Het cachen van resultaten of het structureren van code om bestaanscontroles te minimaliseren kan prestaties optimaliseren.

Een alternatief, vooral relevant in Unix-achtige systemen, is het gebruik van `exec()` met een systeemcommando zoals `ls` of `test -d`, maar dit introduceert de overhead van het aanroepen van een shell en is minder draagbaar.

## Zie ook

- [PHP Handleiding: `is_dir()`](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Handleiding: `file_exists()`](https://www.php.net/manual/en/function.file-exists.php)
- [Bestandssysteem best practices in PHP](https://www.php-fig.org/psr/psr-4/)
- [PHP bestandssysteem functies](https://www.php.net/manual/en/ref.filesystem.php)
