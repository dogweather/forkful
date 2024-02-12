---
title:                "Quotes verwijderen uit een string"
aliases:
- /nl/fish-shell/removing-quotes-from-a-string.md
date:                  2024-01-28T22:06:10.928067-07:00
model:                 gpt-4-0125-preview
simple_title:         "Quotes verwijderen uit een string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van aanhalingstekens uit een string gaat over het strippen van die lastige enkele (' ') of dubbele (" ") aanhalingstekens van je tekstgegevens. Programmeurs doen dit vaak om invoer te ontsmetten of gegevens voor te bereiden voor verdere verwerking zonder de rommel van aanhalingstekens.

## Hoe te:

Fish heeft ingebouwde magie voor dit soort taak. Gebruik de `string` functie zonder een druppel zweet. Bekijk deze spreuken:

```fish
# Voorbeeld met enkele aanhalingstekens
set quoted "'Hallo, Wereld!'"
set unquoted (string trim --chars \"\'\" $quoted)
echo $unquoted # Uitvoer: Hallo, Wereld!

# Hetzelfde geldt voor dubbele aanhalingstekens
set double_quoted "\"Hallo, Universum!\""
set unquoted (string trim --chars \"\'\" $double_quoted)
echo $unquoted # Uitvoer: Hallo, Universum!
```

## Diepere Duik

Terug in het stenen tijdperk van de opdrachtregel, zou je worstelen met `sed` of `awk` om aanhalingstekens te verwijderen; een ware warboel van backslashes en cryptische vlaggen. Fish's `string` functie komt uit een nieuwere tijd, waardoor code schoner en intuïtiever wordt.

Alternatieven in andere shells kunnen nog steeds afhankelijk zijn van deze oude tools of kunnen hun eigen ingebouwde methoden gebruiken, zoals bash's parameteruitbreiding of zsh's modificatoren.

De `string` functie gaat verder dan het trimmen van aanhalingstekens. Het is een Zwitsers zakmes voor stringbewerkingen in Fish. Met `string` kun je strings snijden, splitsen, samenvoegen, of zelfs regex-matching van strings direct in je terminal doen.

## Zie Ook

Duik dieper in `string` met de hulp van de officiële documentatie:
- [Fish Shell String Documentatie](https://fishshell.com/docs/current/commands.html#string)

Voor nostalgie of wanneer je scriptt met meer traditionele shells, bekijk:
- [Sed & Awk Gids](https://www.grymoire.com/Unix/Sed.html)
- [Bash Parameteruitbreiding](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
