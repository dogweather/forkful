---
title:                "Code organiseren in functies"
aliases:
- /nl/fish-shell/organizing-code-into-functions.md
date:                  2024-01-28T23:01:27.107827-07:00
model:                 gpt-4-0125-preview
simple_title:         "Code organiseren in functies"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/fish-shell/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, dogweather, reviewed and added links
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het organiseren van code in functies gaat over het bundelen van stukjes script om specifieke taken uit te voeren. We doen dit omdat het de code makkelijker maakt om te lezen, te testen en te hergebruiken — niemand wil door een moeras van codespaghetti waden.

## Hoe:
In Fish schrijf je een functie met het trefwoord `function`, geef je het een naam en eindig je met `end`. Hier is een eenvoudige:

```fish
function hello
    echo "Hallo, wereld!"
end

hello
```

Output:
```
Hallo, wereld!
```

Laten we het nu een gebruiker laten begroeten:

```fish
function greet
    set user (whoami)
    echo "Hey daar, $user!"
end

greet
```

Output:
```
Hey daar, jouw_gebruikersnaam!
```

Om het over sessies heen te bewaren, gebruik je `funcsave greet`.

## Diepere Duik
Fish Shell-functies zijn als mini-scripts — je kunt er vrijwel alles in stoppen. Historisch gezien heeft het concept van functies in shellscripts ontelbaar veel uren aan repetitief typen en debuggen bespaard. In tegenstelling tot programmeertalen zoals Python, zijn Shell-functies meer gericht op gemak dan op structuur.

Sommige shells, zoals Bash, gebruiken `function` of gewoon rechte haakjes. Fish houdt het op `function ... end` — duidelijk en leesbaar. Binnen Fish-functies krijg je alle toeters en bellen: parameters, lokale variabelen met `set -l`, en je kunt zelfs een functie binnen een andere functie definiëren.

Je hebt geen `return` waarde nodig omdat Fish daar niet sterk op inzet; de output van je functie is de return. En als je persistente functies wilt die beschikbaar zijn voor toekomstige sessies, onthoud dan `funcsave`.

## Zie Ook

- De Fish tutorial over functies: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### Functie-commando's

- [function](https://fishshell.com/docs/current/cmds/function.html) — Maak een functie
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — Print of verwijder functies
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — Sla de definitie van een functie op in de autolaadmap van de gebruiker
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — Bewerk een functie interactief
