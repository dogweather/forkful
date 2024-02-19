---
aliases:
- /nl/python/extracting-substrings/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:49.328173-07:00
description: "Substrings extraheren betekent specifieke delen van een string eruit\
  \ trekken, zoals een stukje van een lint knippen. Programmeurs doen dit om gegevens\
  \ te\u2026"
lastmod: 2024-02-18 23:09:01.421246
model: gpt-4-0125-preview
summary: "Substrings extraheren betekent specifieke delen van een string eruit trekken,\
  \ zoals een stukje van een lint knippen. Programmeurs doen dit om gegevens te\u2026"
title: Substrings extraheren
---

{{< edit_this_page >}}

## Wat & Waarom?
Substrings extraheren betekent specifieke delen van een string eruit trekken, zoals een stukje van een lint knippen. Programmeurs doen dit om gegevens te isoleren, informatie te parseren of simpelweg tekst te manipuleren.

## Hoe te:
```Python
# Met slice-notatie
tekst = "Python rocks!"
substring = tekst[7:12]
print(substring)  # Uitvoer: rocks

# Met de slice()-functie
slice_object = slice(7, 12)
print(tekst[slice_object])  # Uitvoer: rocks

# Met str.split() en toegang tot het element
delen = tekst.split()
print(delen[1])  # Uitvoer: rocks!
```

## Diepere Duik
Historisch gezien was het concept van stringmanipulatie, inclusief substringextractie, cruciaal in vroege programmeertalen zoals C, waar het een complexere taak was die pointers betrokken. Met Python wordt de eenvoud opgeschroefd naar elf - intuïtiever en minder foutgevoelig.

Python biedt meerdere alternatieven voor het extraheren van substrings. Terwijl de gebruikte voorbeelden de slice-notatie gebruiken die super direct is, kunnen methoden zoals `split()` handig zijn wanneer je te maken hebt met delimiters of witruimte.

Onder de motorkap zijn Python-strings arrays van bytes die Unicode-karakters vertegenwoordigen. Maar in tegenstelling tot arrays in andere talen, zijn Python-strings onveranderlijk, wat betekent dat je ze na creatie niet kunt veranderen. Dit aspect is essentieel bij het begrijpen waarom substringoperaties de oorspronkelijke string niet wijzigen, maar in plaats daarvan een nieuwe creëren.

## Zie Ook
- De Python-documentatie over stringmethoden: https://docs.python.org/3/library/stdtypes.html#string-methods
- Een artikel over meer Python stringoperaties: https://realpython.com/python-strings/
- W3Schools' Python string slicing tutorial: https://www.w3schools.com/python/python_strings_slicing.asp
