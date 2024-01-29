---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:55:27.477782-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een string met hoofdletters schrijven betekent alle karakters omzetten naar hoofdletters. Programmeurs doen dit voor consistentie, leesbaarheid, of om aan bepaalde coderingsnormen te voldoen.

## Hoe:
In Fish zet je een string om naar hoofdletters met het `string upper` commando. Zo doe je dat:

```Fish Shell
set lowercased "fish shell is leuk"
set capitalized (string upper $lowercased)
echo $capitalized
```

Uitvoer:
```
FISH SHELL IS LEUK
```

## Diepere Duik
Historisch gezien wordt het omzetten van strings naar hoofdletters in programmering gebruikt voor het formatteren van uitvoer, het uniform opslaan van gegevens, en voor hoofdletterongevoelige vergelijkingen. Hoewel Fish Shell relatief jong is, putten de functies voor stringmanipulatie inspiratie uit andere Unix shells, wat zorgt voor een leesbaardere syntax en gemak.

Belangrijke punten in de ontwerpfilosofie van Fish omvatten gebruiksvriendelijkheid en het bieden van functies die doen wat je verwacht, vandaar het eenvoudige `string upper` commando. Eerdere shells zouden vereisen dat je echo-commando's naar `tr` pipe of gebruik maakt van programma's als `awk` voor zo'n bewerking, wat minder intuïtief kan zijn voor casual gebruikers.

Alternatieven omvatten het gebruik van `awk`:
```Fish Shell
echo "fish shell is leuk" | awk '{print toupper($0)}'
```

Of `tr`:
```Fish Shell
echo "fish shell is leuk" | tr '[:lower:]' '[:upper:]'
```

Ondanks deze alternatieven is `string upper` in Fish duidelijk en to-the-point, waarmee de historische bagage van cryptische commando-opties en syntax van Unix wordt vermeden. Het omzetten naar hoofdletters in Fish verandert de oorspronkelijke string niet, tenzij je deze expliciet opnieuw toewijst, wat je gegevens beschermt tegen accidentele mutaties.

## Zie Ook
- Fish documentatie over stringmanipulatie: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Een korte geschiedenis van Unix tekstverwerkingstools: [Unix Text Processing (O'Reilly)](http://www.oreilly.com)
- Een gids voor stringmanipulatie in Unix shells ter vergelijking: [Greg's Wiki (mywiki.wooledge.org)](http://mywiki.wooledge.org/BashFAQ/099)
