---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:27.262944-07:00
description: 'Hoe te: Stel je voor dat je een script schrijft om het kwadraat en de
  kubus van een getal te berekenen. Zonder functies is het een warboel van herhaling.'
lastmod: '2024-03-13T22:44:50.382188-06:00'
model: gpt-4-0125-preview
summary: Stel je voor dat je een script schrijft om het kwadraat en de kubus van een
  getal te berekenen.
title: Code organiseren in functies
weight: 18
---

## Hoe te:
Stel je voor dat je een script schrijft om het kwadraat en de kubus van een getal te berekenen. Zonder functies is het een warboel van herhaling:

```Python
num = 4
kwadraat = num * num
kubus = num * num * num
print(f"Kwadraat: {kwadraat}, Kubus: {kubus}")

num = 5
kwadraat = num * num
kubus = num * num * num
print(f"Kwadraat: {kwadraat}, Kubus: {kubus}")
```
Uitvoer:
```
Kwadraat: 16, Kubus: 64
Kwadraat: 25, Kubus: 125
```

Met functies is het netter:

```Python
def kwadraat(n):
    return n * n

def kubus(n):
    return n ** 3

num = 4
print(f"Kwadraat: {kwadraat(num)}, Kubus: {kubus(num)}")

num = 5
print(f"Kwadraat: {kwadraat(num)}, Kubus: {kubus(num)}")
```
Uitvoer:
```
Kwadraat: 16, Kubus: 64
Kwadraat: 25, Kubus: 125
```

## Diepere duik
Vroeger, toen programma's eenvoudig waren, kon je ermee wegkomen door gewoon een lijst met instructies te schrijven. Maar naarmate de software complexer werd, realiseerden ontwikkelaars zich dat ze steeds opnieuw dezelfde code schreven. Hallo, functies—herbruikbare blokken code die één enkele actie uitvoeren.

Alternatieven voor functies zijn klassen (functies bundelen met gegevens waarop ze opereren) en inline code (intelligentie precies waar je het nodig hebt, maar riskant voor complexe taken). Wat betreft de implementatie, is de truc niet alleen om functies te creëren, maar om ze goed één ding te laten doen—denk aan het principe van enkele verantwoordelijkheid. Functies zouden ook idealiter staatloos moeten zijn, wat betekent geen verrassingen met data die binnenkomt of eruit gaat.

## Zie ook
- De officiële Python-handleidingen over functies: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Schone Code' van Robert C. Martin, voor principes over hoe schone functies te schrijven.
- 'Refactoring: Het verbeteren van het ontwerp van bestaande code' van Martin Fowler, wat voorbeelden bevat van het organiseren van code.
