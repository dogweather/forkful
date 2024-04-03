---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:32.615746-07:00
description: "Tekst zoeken en vervangen is hoe je specifieke stringen in inhoud vindt\
  \ en ze omwisselt voor iets anders. Programmeurs doen dit om gegevens bij te werken,\u2026"
lastmod: '2024-03-13T22:44:50.879623-06:00'
model: gpt-4-0125-preview
summary: Tekst zoeken en vervangen is hoe je specifieke stringen in inhoud vindt en
  ze omwisselt voor iets anders.
title: Tekst zoeken en vervangen
weight: 10
---

## Hoe:
Hier is een snelle manier om 'kat' door 'hond' te vervangen in een zin met PHP:

```PHP
<?php
$text = 'De snelle bruine vos springt over de luie kat';
$vervangenTekst = str_replace('kat', 'hond', $text);

echo $vervangenTekst;
?>
```

Voorbeelduitvoer:

```
De snelle bruine vos springt over de luie hond
```

Stel nu dat we te maken hebben met hoofdletterongevoelige vervanging:

```PHP
<?php
$text = 'Katapulten zijn CATegorisch geweldig!';
$vervangenTekst = str_ireplace('kat', 'hond', $text);

echo $vervangenTekst;
?>
```

Voorbeelduitvoer:

```
Hondapulten zijn HONDegorisch geweldig!
```

## Diepere duik:
Zoek- en vervangfuncties bestaan al sinds de vroege dagen van het computergebruik — denk aan `sed` in Unix. In PHP zijn `str_replace` en `str_ireplace` je beste opties voor een simpele zoek en vervang. `str_replace` is hoofdlettergevoelig, terwijl `str_ireplace` dat niet is.

Hoe werken ze? Onder de motorkap controleren beide functies elk deel van de string, zoeken naar overeenkomsten en vervangen deze. Ze kunnen ook omgaan met arrays, dus je kunt meerdere patronen in één keer zoeken en vervangen.

Nu, als je meer controle nodig hebt, zoals patroonafstemming, wil je `preg_replace` gebruiken. Dit maakt gebruik van reguliere expressies, waardoor je veel meer flexibiliteit en precisie krijgt:

```PHP
<?php
$text = 'De snelle bruine vos springt over de luie kat 7 keer.';
$vervangenTekst = preg_replace('/\bkat\b/i', 'hond', $text);

echo $vervangenTekst;
?>
```

Voorbeelduitvoer:

```
De snelle bruine vos springt over de luie hond 7 keer.
```

Dit vervangt 'kat' door 'hond', negeert hoofdlettergebruik (`/i` modifier), en komt overeen met hele woorden alleen (`\b` woordgrens).

## Zie ook:
- Officiële PHP Documentatie over str_replace: https://www.php.net/manual/nl/function.str-replace.php
- Officiële PHP Documentatie over str_ireplace: https://www.php.net/manual/nl/function.str-ireplace.php
- Officiële PHP Documentatie over preg_replace: https://www.php.net/manual/nl/function.preg-replace.php
- Tutorial over reguliere expressies: https://www.regular-expressions.info/
- Unix `sed` stream-editor om tekst te filteren en te transformeren: http://www.grymoire.com/Unix/Sed.html
