---
title:                "Substrings extraheren"
aliases:
- nl/php/extracting-substrings.md
date:                  2024-01-28T21:59:51.683853-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/php/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het extraheren van substrings betekent het trekken van specifieke delen uit een string. Programmeurs doen dit om gegevens te verwerken of te manipuleren, zoals het ophalen van een gebruikersnaam uit een e-mailadres of een bestandsextensie uit een bestandsnaam.

## Hoe:
PHP biedt verschillende functies om substrings te extraheren. Laten we `substr`, `mb_substr` en `strstr` bekijken.

```PHP
$string = "Hallo, Wereld! Programmeren is leuk.";

// 'Wereld' extraheren met substr.
echo substr($string, 7, 5); // Output: Wereld

// UTF-8-stringvoorbeeld met mb_substr voor multibyte-karakters.
$utf8String = "こんにちは世界";
echo mb_substr($utf8String, 5, 2); // Output: 世

// Alles na de komma krijgen met strstr.
echo strstr($string, ","); // Output: , Wereld! Programmeren is leuk.
```

## Dieper Duiken
In de vroege dagen van PHP was de hoofdmanier om een stuk van een string te pakken `substr()`. Echter, `substr()` had (en heeft nog steeds) een limiet: het werkt niet goed samen met niet-Engelse karakters (zoals Japans of Arabisch).

Hier komt `mb_substr()`, de multibyte-veilige tegenhanger die karakters van verschillende coderingen respecteert. Het zorgt ervoor dat wanneer je een substring pakt, je niet door het midden van een karakter in bytes scheurt, wat cruciaal is voor internationale toepassingen.

`strstr()`, aan de andere kant, vindt de eerste voorkomen van een substring en geeft je alles erna. Er is ook `strchr()` dat een alias is van `strstr()`.

Terwijl `substr()` en `mb_substr()` je toestaan om precies te specificeren waar je moet beginnen en hoeveel je moet nemen, is `strstr()` meer een "vind en geef me de rest" tool.

## Zie Ook
Hier is wat extra leesvoer als je meer wilt weten:

- Officiële PHP-documentatie voor stringfuncties: https://www.php.net/manual/en/ref.strings.php
- Een diepgaande duik in PHP's multibyte stringfuncties: https://www.php.net/manual/en/book.mbstring.php
- Meer over karaktercodering en waarom het belangrijk is: http://kunststube.net/encoding/
