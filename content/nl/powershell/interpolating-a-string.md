---
title:                "Een string interpoleren"
aliases:
- nl/powershell/interpolating-a-string.md
date:                  2024-01-28T22:02:17.456291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/powershell/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
String interpolatie schuift waarden in een stringtemplate zoals stukken van een puzzel, waardoor strings dynamisch worden en de code schoner. Programmeurs gebruiken het om variabelen, expressies en opmaak direct binnen strings in te voegen, wat de rommel van concatenatie vermindert.

## Hoe te:
In PowerShell, gebruik je interpolatie met dubbel aangehaalde strings en het `$` symbool voor de variabelenaam. Omring expressies met `$()` om ze direct binnen de string te evalueren.

```PowerShell
$name = "Alex"
$day = (Get-Date).DayOfWeek

# Basis variabele interpolatie
"Hallo, $name! Fijne $dag!"

# Expressie interpolatie
"Pi tot twee decimalen is $(Math::Round([Math]::Pi, 2))"

# Uitvoer
Hallo, Alex! Fijne woensdag!
Pi tot twee decimalen is 3.14
```

## Diepgaand
PowerShell heeft string interpolatie overgenomen van eerdere programmeertalen zoals Perl. Voor PowerShell v3, concateerden we met de `+` operator of gebruikten we de `-f` formaat operator. Hier is de evolutie:

- Oude-school concatenatie: `"Hallo, " + $name + "! Het is " + $day + "."`
- Formaat operator: `"Hallo, {0}! Het is {1}." -f $name, $day`

Geïnterpoleerde strings zijn gemakkelijker te lezen en minder foutgevoelig. Achter de schermen interpreteert PowerShell de geïnterpoleerde string en vervangt variabelen of expressies met hun waarden wanneer de string wordt geëvalueerd, niet wanneer deze wordt gedefinieerd.

## Zie Ook
- [Uitleg formaat operator](https://ss64.com/ps/syntax-f-operator.html)
