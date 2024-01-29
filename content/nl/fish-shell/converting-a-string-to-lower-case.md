---
title:                "Een string omzetten naar kleine letters"
date:                  2024-01-28T21:57:30.622145-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string omzetten naar kleine letters"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het omzetten van een string naar kleine letters verandert alle letters in die string naar hun kleine lettervorm. Programmeurs doen dit voor consistentie, vergelijking, sortering of om te voldoen aan de hoofdlettergevoeligheidsvereisten van sommige systemen.

## Hoe:

Met behulp van het `string` commando is tekst omzetten naar kleine letters eenvoudig. Doe gewoon:

```Fish Shell
echo "MAKE ME LOWERCASE" | string lower
```

Voorbeelduitvoer:

```
make me lowercase
```

Voor een variabele:

```Fish Shell
set my_string "SHOUTY CASE TEXT"
string lower -q -- $my_string
```

Uitvoer:

```
shouty case text
```

## Diepgaande Duik:

Voor Fish Shell gebruikten Unix-gebruikers vaak `tr '[:upper:]' '[:lower:]'` of `awk '{print tolower($0)}'`. Hoewel deze werken, zijn ze niet zo schoon of eenvoudig als de ingebouwde `string lower` functie van Fish.

Fish introduceerde `string` in v2.3.0 (mei 2016), waardoor stringmanipulatie een kernonderdeel van de shell werd, in plaats van dat er externe commando's nodig waren. Dit voegde eenvoud en snelheid toe aan veelvoorkomende taken zoals hoofdletteromzetting.

Waarom niet gewoon `tr` of `awk` gebruiken? `string lower` is ingebouwd in Fish, wat betekent dat het sneller is (er worden geen nieuwe processen gestart) en werkt op een consistente en voorspelbare manier op verschillende systemen. Het maakt ook deel uit van een breder `string` commandosuite dat andere stringbewerkingen afhandelt, wat het schrijven van scripts netter en efficiënter kan maken.

## Zie Ook:

- Officiële documentatie voor `string`: https://fishshell.com/docs/current/cmds/string.html
- Fish Shell GitHub-repository: https://github.com/fish-shell/fish-shell
- De historische context en vergelijking van `string` versus traditionele Unix-commando's: https://github.com/fish-shell/fish-shell/issues/159
