---
title:                "Code organiseren in functies"
date:                  2024-01-28T22:02:51.394504-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code organiseren in functies"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Code organiseren in functies gaat over het bundelen van stukjes script om specifieke taken uit te voeren. We doen dit omdat het de code makkelijker te lezen, testen en hergebruiken maakt - niemand wil waden door een moeras van codespaghetti.

## Hoe te:
In Fish schrijf je een functie met het trefwoord `function`, geef je het een naam en eindig je met `end`. Hier is een eenvoudige:

```fish
function hello
    echo "Hallo, wereld!"
end

hello
```

Uitvoer:
```
Hallo, wereld!
```

Laten we het nu een gebruiker laten begroeten:

```fish
function greet
    set user (whoami)
    echo "Hé daar, $user!"
end

greet
```

Uitvoer:
```
Hé daar, jouw_gebruikersnaam!
```

Om het over sessies te bewaren, gebruik je `funcsave greet`.

## Diepere Duik
Fish Shell-functies zijn als mini-scripts - je kunt er vrijwel alles in stoppen. Historisch gezien heeft het concept van functies in shell scripting talloze uren aan herhalend typen en debuggen bespaard. In tegenstelling tot programmeertalen zoals Python, gaan Shell-functies meer over gemak dan structuur.

Sommige shells, zoals Bash, gebruiken `function` of gewoon rechte haakjes. Fish houdt het bij `function ... end` - duidelijk en leesbaar. Binnen Fish-functies krijg je alle toeters en bellen: parameters, lokale variabelen met `set -l`, en je kunt zelfs een functie binnen een andere functie definiëren.

Je hebt geen `return` waarde nodig omdat Fish daar niet veel om geeft; de output van je functie is zijn return. En als je persistente functies wilt die beschikbaar zijn voor toekomstige sessies, onthoud dan `funcsave`.

## Zie Ook
- De Fish tutorial over functies: https://fishshell.com/docs/current/tutorial.html#tut_functions
- De Fish documentatie voor `function`: https://fishshell.com/docs/current/cmds/function.html
- Een uitgebreide gids over het schrijven van functies in Fish: https://fishshell.com/docs/current/index.html#syntax-function
