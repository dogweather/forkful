---
title:                "Debug-output afdrukken"
aliases:
- /nl/python/printing-debug-output/
date:                  2024-01-28T22:05:04.743713-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debug-output afdrukken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Debug output printen is alsof je een gesprek hebt met je code om uit te vinden wat het denkt. Programmeurs doen dit om de gremlins op te sporen die kattenkwaad uithalen in hun programma's.

## Hoe:
Eenvoudig en simpel, je print dingen om te zien wat er aan de hand is. Hier is de klassieker:

```Python
print("Hallo, bug hunters!")
```

Voel je je al een detective? Laten we nu eens kijken hoe je variabelen zich gedragen:

```Python
buggy_number = 42
print(f"Debug: Het nummer is {buggy_number}")
```

Als het ingewikkeld wordt, wil je misschien een kijkje nemen in een lijst:

```Python
buggy_list = [1, 2, 3]
print(f"Debug: De lijst bevat {buggy_list}")
```

Voer deze snippets uit, en je output is dit:

```
Hallo, bug hunters!
Debug: Het nummer is 42
Debug: De lijst bevat [1, 2, 3]
```

## Diepere Duik
Debuggen door te printen heeft een lange stamboom, die teruggaat tot de tijd dat dinosaurussen de aarde bewandelden (ook bekend als de vroege dagen van computing). Het is eenvoudig en universeel toepasbaar omdat het simpelweg uitvoert wat je wilt controleren.

Hoewel `print()` de snelle en vuile tool in Python is, bestaan er alternatieven. Voor echt speurwerk wil je misschien loggen gebruiken met verschillende niveaus zoals DEBUG, INFO, WAARSCHUWING, enz. Op deze manier kun je bepalen wat wordt geprint en wat wordt gedempt.

Soms hoor je over fancy debuggers die je de tijd laten stoppen (soort van) en rondsnuffelen in je code terwijl het draait. Ze zijn super krachtig en de moeite waard om te leren, maar laat ze je niet slecht voelen voor het hier en daar gooien van een snelle `print()`.

Wat betreft de implementatie, de eenvoud van `print()` is zijn schoonheid. Onthoud gewoon dat constant printen naar de console je kan vertragen als je het een ziljoen keer doet in een lus. En, het kan super snel rommelig worden. Commentaar of verwijder die regels zodra je die bugs hebt vastgespijkerd.

## Zie Ook
Voor meer over printen en debuggen in Python:
- Python's ingebouwde `print()` functie: [Python docs over print](https://docs.python.org/3/library/functions.html#print)
- Python Logging: [Logging HOWTO](https://docs.python.org/3/howto/logging.html)
- Voor de liefhebbers van debuggers: [Python docs over pdb](https://docs.python.org/3/library/pdb.html)
