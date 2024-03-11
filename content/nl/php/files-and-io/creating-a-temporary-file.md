---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:32.575950-07:00
description: "Het aanmaken van een tijdelijk bestand in PHP betekent dat je een bestand\
  \ maakt dat net lang genoeg blijft bestaan om te gebruiken, en dan poef\u2014het\
  \ is\u2026"
lastmod: '2024-03-11T00:14:24.746024-06:00'
model: gpt-4-0125-preview
summary: "Het aanmaken van een tijdelijk bestand in PHP betekent dat je een bestand\
  \ maakt dat net lang genoeg blijft bestaan om te gebruiken, en dan poef\u2014het\
  \ is\u2026"
title: Een tijdelijk bestand aanmaken
---

{{< edit_this_page >}}

## Wat & Waarom?
Het aanmaken van een tijdelijk bestand in PHP betekent dat je een bestand maakt dat net lang genoeg blijft bestaan om te gebruiken, en dan poef—het is weg. Waarom zou je dat doen? Het is ideaal voor het verwerken van gegevensblokken tijdens de verwerking, het bewaren van gevoelige informatie buiten de schijf, en het zorgen dat er geen sporen achterblijven nadat je script is afgerond.

## Hoe:
PHP helpt je bij het aanmaken van tijdelijke bestanden met de `tmpfile()` functie, die voor jou een bestand maakt in de tijdelijke directory van je systeem. Hier is een snel voorbeeld:

```PHP
<?php
$tempFile = tmpfile();
fwrite($tempFile, "Hallo, tijdelijke wereld!");
rewind($tempFile);

echo fread($tempFile, 1024); // Lees wat we naar het bestand hebben geschreven

fclose($tempFile); // Het tijdelijke bestand wordt automatisch verwijderd
?>
```

Voorbeelduitvoer:
```
Hallo, tijdelijke wereld!
```

Je kunt ook `tempnam()` gebruiken om een bestandsnaam te krijgen die je zelf kunt beheren:

```PHP
<?php
$tempFilePath = tempnam(sys_get_temp_dir(), 'Tux');
file_put_contents($tempFilePath, "Pinguïns zijn cool!");

echo file_get_contents($tempFilePath); // Lees de inhoud

unlink($tempFilePath); // Verwijder het bestand als je klaar bent
?>
```

Voorbeelduitvoer:
```
Pinguïns zijn cool!
```

## Diepere duik
De `tmpfile()` functie bestaat al in PHP sinds de vroege dagen. Het handelt de bestandscreatie en -opruiming voor je af, waardoor mogelijke beveiligingsrisico's van het rondhangen van gevoelige gegevens netjes worden omzeild.

Aan de andere kant geeft `tempnam()` je slechts een naam, waardoor het bestandsbeheer in jouw handen ligt. Een waarschuwing: vergeet niet om het bestand te `unlink()` wanneer je klaar bent.

Deze tijdelijke bestanden worden doorgaans opgeslagen in de standaard tijdelijke directory van je systeem, die je kunt vinden met `sys_get_temp_dir()`. Deze locatie kan variëren afhankelijk van je besturingssysteem en omgevingsconfiguratie.

Je hebt ook alternatieven zoals `tempnam()` en `tmpfile()`, en er is het chiquere `sys_get_temp_dir()` voor het verkrijgen van die ongrijpbare tijdelijke directory. Maar onthoud de gouden regel met betrekking tot tijdelijke bestanden: ruim op na jezelf—PHP doet dit voor een deel automatisch, maar het is een goede gewoonte om expliciet te zijn.

## Zie Ook
- [De officiële PHP-documentatie voor tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [PHP-handleiding over de tempnam() functie](https://www.php.net/manual/en/function.tempnam.php)
- [PHP.net's informatie over sys_get_temp_dir()](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [Bestandssysteembeveiliging](https://www.php.net/manual/en/security.filesystem.php)
