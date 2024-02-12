---
title:                "Fouten afhandelen"
aliases: - /nl/python/handling-errors.md
date:                  2024-01-28T22:02:23.666645-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het afhandelen van fouten in Python (of elke programmeertaal) gaat over het verwachten van het onverwachte - het is de kunst van het gracieus omgaan met situaties waarin dingen misgaan in je code. We doen dit om crashes te voorkomen, gebruikers te begeleiden en onze programma's robuust en betrouwbaar te maken.

## Hoe:

``` Python
# Basis try-except blok
try:
    # risicovolle code
    number = int(input("Voer een nummer in: "))
except ValueError:
    # afhandelen van fout
    print("Dat is geen nummer!")

# Meerdere uitzonderingen specificeren
try:
    # code die verschillende uitzonderingen kan veroorzaken
    result = 10 / int(input("Voer een deler in: "))
except ZeroDivisionError:
    print("Oeps! Kan niet delen door nul.")
except ValueError:
    print("Ik heb een nummer nodig, maatje.")

# Gebruik van else en finally
try:
    number = int(input("Voer een nummer in voor het kwadrateren: "))
except ValueError:
    print("Ik zei een nummer!")
else:
    # geen fouten opgetreden
    print("Jouw nummer kwadraat is:", number**2)
finally:
    # wordt altijd uitgevoerd
    print("Bedankt voor het proberen!")
```

Voorbeelduitvoer bij het invoeren van een ongeldig nummer voor het eerste blok:
```
Voer een nummer in: hallo
Dat is geen nummer!
```

## Diepere Duik

Sinds de dageraad van het programmeren is foutafhandeling cruciaal geweest. Eerste benaderingen waren rudimentair, zoals het controleren van condities voor elke risicovolle operatie. Pythons `try-except` syntax komt uit een erfgoed van uitzonderingsafhandeling in oudere talen zoals C++ en Java, wat het proces vereenvoudigt.

Wanneer je een blok code `probeert`, let Python op eventuele uitzonderingen. Als er een fout optreedt, vangt het `except` blok deze. Je kunt specifiek zijn over de uitzonderingen die je vangt of ze allemaal vangen met een kale `except`. Echter, specifiek zijn is de betere aanpak - het is precies, geen vangnet dat alles opvangt.

`else` en `finally` zijn extra's bij dit concept. Het `else` blok wordt uitgevoerd als het try-blok foutloos is. `finally` is de betrouwbare vriend die hoe dan ook uitgevoerd wordt - denk aan opruimoperaties.

Alternatieven? Die zijn er zeker. Sommige talen gebruiken retourcodes in plaats van uitzonderingen. Je kunt ook `with`-instructies tegenkomen voor het beheren van middelen of `assertions` die condities controleren tijdens de ontwikkeling. Maar als we het hebben over solide strategieën voor foutafhandeling, valt het try-catch model op vanwege zijn leesbaarheid en structuur.

## Zie Ook

Hier zijn enkele goede aanvullende bronnen om nog dieper te duiken:

- De officiële documentatie van Python over fouten en uitzonderingen: [Python Docs – Fouten en Uitzonderingen](https://docs.python.org/3/tutorial/errors.html)
- De gids van Real Python over dit onderwerp: [Real Python - Het try/except/else/finally blok](https://realpython.com/python-exceptions/)
- Een doordachte discussie over de beste praktijken voor foutafhandeling: [Stack Overflow – Hoe negeer ik op de juiste manier uitzonderingen?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
