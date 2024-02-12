---
title:                "Refactoring"
aliases:
- /nl/python/refactoring.md
date:                  2024-01-28T22:06:26.585422-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is het proces van het herstructureren van bestaande computercod—het wijzigen van de factoring—zonder het externe gedrag te veranderen. Programmeurs doen dit om code op te schonen, de leesbaarheid te verbeteren en het makkelijker te onderhouden en uit te breiden, allemaal zonder nieuwe functies toe te voegen.

## Hoe:
Stel je hebt een stukje code dat de oppervlakte en de omtrek van een rechthoek berekent en afdrukt, gegeven de lengte en breedte. Het doet zijn werk, maar het is repetitief en een beetje rommelig.

```python
# Originele Versie
lengte = 4
breedte = 3

# Bereken oppervlakte en omtrek
oppervlakte = lengte * breedte
omtrek = 2 * (lengte + breedte)

print("Oppervlakte:", oppervlakte)
print("Omtrek:", omtrek)
```

We kunnen dit refactoren door de functionaliteit in functies te encapsuleren, wat de code meer georganiseerd en herbruikbaar maakt:

```python
# Gerefactorde Versie

def bereken_oppervlakte(lengte, breedte):
    return lengte * breedte

def bereken_omtrek(lengte, breedte):
    return 2 * (lengte + breedte)

# gebruik
lengte = 4
breedte = 3

print("Oppervlakte:", bereken_oppervlakte(lengte, breedte))
print("Omtrek:", bereken_omtrek(lengte, breedte))
```

Beide fragmenten geven hetzelfde resultaat:
```
Oppervlakte: 12
Omtrek: 14
```

Maar de gerefactorde versie is schoner en scheidt de zorgen, wat het makkelijker maakt om een berekening te updaten zonder de andere te beïnvloeden.

## Diepgaande Duik
Refactoring vindt zijn oorsprong in de vroege dagen van software engineering, toen programmeurs zich realiseerden dat code kon—en zou moeten—worden verbeterd, zelfs als deze al "werkt". Het baanbrekende boek van Martin Fowler, "Refactoring: Improving the Design of Existing Code", formuleerde veel kernprincipes en technieken. Hij zei beroemd: "Iedere dwaas kan code schrijven die een computer kan begrijpen. Goede programmeurs schrijven code die mensen kunnen begrijpen."

Alternatieven voor refactoring kunnen onder meer het herschrijven van code vanaf nul of het maken van kleine aanpassingen zonder systematische verbetering omvatten. Echter, refactoring is meestal kosteneffectiever dan een herschrijving en minder riskant dan ad-hoc wijzigingen. Implementatiedetails kunnen specifiek zijn voor elk programmeerparadigma; echter, objectgeoriënteerd programmeren leent zich met name goed voor refactoring, vooral met technieken zoals het extraheren van methoden (zoals onze `bereken_oppervlakte` en `bereken_omtrek` functies), inlining, het verplaatsen van functies tussen objecten, en het hernoemen van methoden of variabelen voor duidelijkheid.

Refactoring in Python maakt vaak gebruik van tools zoals `PyCharm`, dat ingebouwde refactoring mogelijkheden heeft, of `rope`, een Pythonbibliotheek die speciaal is ontworpen voor refactoring. Zorgvuldig gebruik van versiebeheer, zoals `git`, tijdens refactoring wordt sterk aanbevolen om wijzigingen stapsgewijs bij te houden.

## Zie Ook
Voor degenen die hongerig zijn naar meer, duik in de volgende bronnen:
- Het boek van Martin Fowler: [Refactoring: Improving the Design of Existing Code](http://www.refactoring.com/)
- Python refactoring met `rope`: [GitHub - rope](https://github.com/python-rope/rope)
- PyCharm refactoring documentatie: [Jetbrains PyCharm Refactoring Broncode](https://www.jetbrains.com/help/pycharm/refactoring-source-code.html)
- Refactoring.guru: [Refactoring and Design Patterns](https://refactoring.guru/refactoring)
- Clean Code lezingen door Uncle Bob (Robert C. Martin): [Clean Code - Uncle Bob / Les 1](https://www.youtube.com/watch?v=7EmboKQH8lM)
