---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:47.050144-07:00
description: "In PHP betekent het converteren van een string naar kleine letters dat\
  \ alle alfabetische karakters in een string worden getransformeerd naar hun kleine\u2026"
lastmod: '2024-02-25T18:49:48.219365-07:00'
model: gpt-4-0125-preview
summary: "In PHP betekent het converteren van een string naar kleine letters dat alle\
  \ alfabetische karakters in een string worden getransformeerd naar hun kleine\u2026"
title: Een string omzetten naar kleine letters
---

{{< edit_this_page >}}

## Wat & Waarom?

In PHP betekent het converteren van een string naar kleine letters dat alle alfabetische karakters in een string worden getransformeerd naar hun kleine lettervariant. Programmeurs doen dit voor consistentie, vooral bij het vergelijken of sorteren van strings, waar hoofdlettergevoeligheid dingen in de war kan schoppen.

## Hoe:

PHP gebruikt `strtolower` om alle karakters in een string naar kleine letters te maken. Zo werkt het:

```php
<?php
$originalString = "HeLLo WoRLD!";
$lowerCaseString = strtolower($originalString);

echo $lowerCaseString; // Geeft uit: hello world!
?>
```

Als je te maken hebt met multibyte karaktercoderingen, zoals UTF-8, gebruik dan `mb_strtolower` in plaats daarvan:

```php
<?php
$originalString = "İstanbul";
$lowerCaseString = mb_strtolower($originalString, 'UTF-8');

echo $lowerCaseString; // Geeft uit: istanbul (converteert İ correct naar i)
?>
```

## Diepgaand

Historisch gezien is de `strtolower`-functie van PHP de standaardfunctie geweest voor conversie van hoofdletters naar kleine letters, geïntroduceerd in de zeer vroege versies van PHP. Echter, naarmate PHP-applicaties wereldwijder werden, kwam de noodzaak om multibyte karaktercoderingen correct te kunnen verwerken, wat leidde tot `mb_strtolower`.

Alternatieven voor `strtolower` en `mb_strtolower` omvatten het gebruik van reguliere expressies met de `mb_ereg_replace_callback`-functie of `preg_replace_callback`, maar voor eenvoudige conversie naar kleine letters zijn ze te ingewikkeld.

In PHP zijn strings traditioneel op byte gebaseerd, niet op karakter gebaseerd, wat betekent dat elke byte één karakter is. Dit werkt voor enkele-bytecoderingen zoals ASCII, waar elk karakter inderdaad één byte is. Voor multibyte coderingen begrijpt `mb_strtolower` karaktercodering en behandelt tekens zoals ze behandeld moeten worden.

## Zie Ook

- PHP-handleiding over `strtolower`: https://www.php.net/manual/nl/function.strtolower.php
- PHP-handleiding over `mb_strtolower`: https://www.php.net/manual/nl/function.mb-strtolower.php
- UTF-8 en Unicode voor PHP-ontwikkelaars: https://www.php.net/manual/nl/book.mbstring.php
