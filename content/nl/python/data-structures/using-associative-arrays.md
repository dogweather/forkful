---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:47.460033-07:00
description: "Associatieve arrays, in Python bekend als woordenboeken, koppelen sleutels\
  \ aan waarden, waardoor het gemakkelijk is om data op te halen, te wijzigen of te\u2026"
lastmod: '2024-03-13T22:44:50.369280-06:00'
model: gpt-4-0125-preview
summary: Associatieve arrays, in Python bekend als woordenboeken, koppelen sleutels
  aan waarden, waardoor het gemakkelijk is om data op te halen, te wijzigen of te
  volgen aan de hand van een unieke identificatie.
title: Gebruik van associatieve arrays
weight: 15
---

## Hoe te:
Een woordenboek in Python maken is eenvoudig. Je sluit sleutel-waardeparen in met accolades `{}`, met sleutels en waarden gescheiden door een dubbele punt:

```Python
# Maak een associatieve array (woordenboek) aan
my_dict = {"naam": "John", "leeftijd": 30, "stad": "New York"}
print(my_dict)
```

Output:
```
{'naam': 'John', 'leeftijd': 30, 'stad': 'New York'}
```

Toegang krijgen tot een waarde via de sleutel is eenvoudig:

```Python
# Toegang tot een waarde
print(my_dict["naam"])
```

Output:
```
John
```

Elementen toevoegen of bijwerken gebeurt door aan een sleutel een waarde toe te wijzen:

```Python
# Voeg een nieuw sleutel-waardepaar toe
my_dict["email"] = "john@voorbeeld.com"
# Werk een waarde bij
my_dict["leeftijd"] = 31
print(my_dict)
```

Output:
```
{'naam': 'John', 'leeftijd': 31, 'stad': 'New York', 'email': 'john@voorbeeld.com'}
```

Om door de items in het woordenboek te itereren:

```Python
# Iterateer door sleutel-waardeparen
for sleutel, waarde in my_dict.items():
    print(f"{sleutel}: {waarde}")
```

Output:
```
naam: John
leeftijd: 31
stad: New York
email: john@voorbeeld.com
```

## Diepere Duik
Associatieve arrays in Python, of woordenboeken, zijn geïntroduceerd om een datastructuur te bieden voor efficiënte data-toegang en -manipulatie. In tegenstelling tot sequenties, die geïndexeerd zijn door een reeks nummers, worden woordenboeken geïndexeerd door sleutels, wat elk onveranderlijk type kan zijn. Deze ontwerpkeuze maakt woordenboeken ideaal geschikt voor snelle opzoektabels waar sleutels naar unieke waarden leiden.

Historisch gezien zijn Python woordenboeken geïmplementeerd met behulp van een hash-tabel, wat zorgt dat de gemiddelde tijdscomplexiteit voor opzoek-, invoeg- en verwijderoperaties O(1) is. In Python 3.6 en later behouden woordenboeken ook de invoegvolgorde van items, wat de voordelen van hash-tabellen combineert met de voorspelbaarheid van invoegvolgorde gezien in geordende datastructuren.

Hoewel woordenboeken ongelooflijk veelzijdig zijn, kunnen in sommige gespecialiseerde gevallen alternatieven zoals `collections.defaultdict` of `collections.OrderedDict` (voor Python 3.7) de voorkeur krijgen. `defaultdict` is met name handig wanneer je een woordenboek nodig hebt dat een standaardwaarde teruggeeft voor niet-bestaande sleutels, wat bepaalde soorten conditionele logica vereenvoudigt. Echter, met de continue verbetering en evolutie van Python, blijft de ingebouwde woordenboekklasse vaak de eerste keuze voor associatieve arrays vanwege de robuustheid en het gemak dat het biedt direct uit de doos.
