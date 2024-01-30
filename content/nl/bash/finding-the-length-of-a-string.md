---
title:                "De lengte van een string vinden"
date:                  2024-01-28T21:59:54.445466-07:00
model:                 gpt-4-0125-preview
simple_title:         "De lengte van een string vinden"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
De lengte van een string bepalen betekent het tellen van de karakters. Programmeurs doen dit om invoer te valideren, door karakters te loopen, of simpelweg om uitvoer uit te lijnen.

## Hoe doe je dat:
Het `#` symbool klaart de klus in bash. Gebruik het met parameterexpansie. Zo werkt het:

```bash
my_string="Hallo, Wereld!"
string_length=${#my_string}
echo $string_length
```

Voorbeelduitvoer:

```
13
```

## Diepere duik
Vroeger gebruikten mensen `expr` of externe tools zoals `wc -m`, maar ingebouwde Bash-functies veranderden het spel. De syntax `${#var}` is onderdeel van parameterexpansie geïntroduceerd in Bash. Het is snel en efficiënt omdat het geen subshell opstart of een extern programma aanroept.

Alternatieven? Zeker, die zijn er:

- `expr length "$my_string"` geeft je hetzelfde resultaat, maar het is een beetje ouderwets.
- `echo -n $my_string | wc -m` gebruikt `wc` om te tellen, maar dat is overkill voor eenvoudige taken.

Details, details... Wanneer je `${#my_string}` gebruikt, krijg je standaard de lengte in bytes. Als je tekst op de unicode-kant van de straat wandelt, moet je misschien rekening houden met multi-byte karakters. Dat is wanneer dingen complexer worden.

## Zie ook
Duik in de handleidingpagina's met `man bash` om de details van parameterexpansie te begrijpen. Voor degenen die zich verdiepen in het behandelen van strings voorbij de basis ASCII, biedt de Advanced Bash-Scripting Guide enkele inzichten: https://www.tldp.org/LDP/abs/html/. En uit liefde voor het leren, hou https://www.gnu.org/software/bash/manual/ in de gaten voor het laatste nieuws over Bash.