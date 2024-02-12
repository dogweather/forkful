---
title:                "Het gebruik van een interactieve shell (REPL)"
aliases:
- /nl/fish-shell/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:22.235401-07:00
model:                 gpt-4-0125-preview
simple_title:         "Het gebruik van een interactieve shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
REPL, of Read-Eval-Print Loop, is een interactieve programmeeromgeving die individuele gebruikersinvoer neemt, deze uitvoert en het resultaat teruggeeft. Programmeurs gebruiken het voor directe feedback, debugging en snelle experimenten met programmeerconcepten zonder de overhead van het compileren en uitvoeren van een volledig programma.

## Hoe te:
In Fish is de interactieve shell de standaardmodus wanneer je het opstart. Zo ziet het eruit in actie:

```Fish Shell
> set color blue
> echo "De lucht is $color"
De lucht is blauw
```

Je kunt ook ingebouwde functies uitvoeren en spelen met commandosubstituties:

```Fish Shell
> function cheer
      echo "Ga Fish $argv!"
  end
> cheer Coders
Ga Fish Coders!
```

Niet alleen functies definiëren, je kunt ook codefragmenten on-the-fly uitvoeren en de uitvoer direct zien:

```Fish Shell
> math "40 / 2"
20
```

## Diepe Duik
Het concept van REPLs gaat ver terug naar de Lisp-programmeertaal in de jaren 1960. Deze vorm van interactief programmeren zette de benchmark voor omgevingen zoals Python's `ipython` en Ruby's `irb`. Fish zet de trend voort met een focus op gebruiksvriendelijkheid en interactief gebruik.

Fish onderscheidt zich van andere shells zoals Bash doordat het vanaf het begin is ontworpen met interactiviteit in gedachten. Het biedt syntaxiskleuring, autosuggesties en tab-aanvullingen die het krachtig maken om te gebruiken in een REPL-stijl workflow. Beter nog, je commando's worden onthouden en zijn doorzoekbaar, waardoor herhaaldelijk testen een fluitje van een cent is.

Alternatieven voor Fish's REPL kunnen `bash` of `zsh` zijn wanneer deze worden gecombineerd met extensies zoals `bash-completion` of `oh-my-zsh`, maar Fish biedt vaak een rijkere ervaring direct uit de doos.

## Zie Ook:
- Fish Documentatie: https://fishshell.com/docs/current/index.html
- Een interessante vergelijking van Fish vs. andere shells: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Een diepere duik in REPLs: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Interactief programmeren in Lisp, een historische blik: http://www.paulgraham.com/ilisp.html
