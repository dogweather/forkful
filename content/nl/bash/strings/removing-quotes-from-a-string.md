---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:05.605156-07:00
description: "Het verwijderen van aanhalingstekens uit een string houdt in dat de\
  \ aanhalingstekens die de string omsluiten, worden weggestript. Programmeurs willen\
  \ dit\u2026"
lastmod: '2024-03-13T22:44:50.967607-06:00'
model: gpt-4-0125-preview
summary: "Het verwijderen van aanhalingstekens uit een string houdt in dat de aanhalingstekens\
  \ die de string omsluiten, worden weggestript. Programmeurs willen dit\u2026"
title: Quotes verwijderen uit een string
weight: 9
---

## Wat & Waarom?
Het verwijderen van aanhalingstekens uit een string houdt in dat de aanhalingstekens die de string omsluiten, worden weggestript. Programmeurs willen dit vaak doen om invoergegevens te saneren, gegevens voor te bereiden voor vergelijkingsdoeleinden, of om te voldoen aan een specifiek gegevensformaat bij de interactie met andere programma's of systemen.

## Hoe te:
Bash heeft verschillende manieren om aanhalingstekens uit strings te verwijderen. Hier zijn enkele snelle voorbeelden:

```Bash
#!/bin/bash

# Gebruik van variabele substitutie om zowel enkele als dubbele aanhalingstekens te verwijderen
STRING="\"Hallo, Wereld!\""
echo ${STRING//\"}

# Gebruik van `tr` om aanhalingstekens te verwijderen
STRING="'Hallo, Wereld!'"
echo $STRING | tr -d "\'"

# Gebruik van `sed` om aanhalingstekens te verwijderen
STRING="\"Hallo, Wereld!\""
echo $STRING | sed 's/"//g'
```

Voorbeelduitvoer:

```
Hallo, Wereld!
Hallo, Wereld!
Hallo, Wereld!
```

## Uitdieping
Lang geleden waren Unix-commando's zoals `tr` en `sed` de belangrijkste hulpmiddelen voor tekstverwerking. Ze worden vandaag de dag nog steeds gebruikt vanwege hun flexibiliteit en kracht in het hanteren van teksttransformaties zoals het verwijderen van aanhalingstekens. Ze zijn een vast onderdeel van de gereedschapskist van elke shell-scripter.

Bash zelf is sindsdien geëvolueerd en variabele substitutie voegt een extra laag van eenvoud toe voor kleinschalige stringmanipulaties. Het bespaart je het uitvoeren naar externe binaries, wat je scripts een beetje efficiënter maakt.

Hoewel `tr` geweldig is voor het verwijderen van karakters, kan het niet omgaan met complexere patronen. `sed`, aan de andere kant, gebruikt reguliere expressies, dus soms is het overkill en mogelijk langzamer voor eenvoudige bewerkingen.

Het kiezen tussen deze methoden is afhankelijk van je specifieke geval. Als je een verscheidenheid aan aanhalingstekens moet verwijderen en je bent al in de context van een Bash-script, dan is het gebruik van variabele substitutie een fluitje van een cent vanwege de eenvoud. Maar als je tekststromen of meerlijnige data aan het transformeren bent, zijn `tr` en `sed` je beste vrienden.

## Zie ook:
- De GNU Bash handleiding, vooral de secties over Parameteruitbreiding en Shell Parameteruitbreiding: https://www.gnu.org/software/bash/manual/
- De handleiding van het `tr` commando: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- Het overzicht van de `sed` streameditor: https://www.gnu.org/software/sed/manual/sed.html
