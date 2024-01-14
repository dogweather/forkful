---
title:                "Python: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Varför läsa kommandoradsargument i Python?

Att kunna läsa kommandoradsargument i Python är en viktig färdighet för alla som vill kunna skriva mer avancerade och användarvänliga program. Genom att läsa och använda kommandoradsargument kan du ge dina användare mer kontroll över programmet och göra det lättare att använda för olika ändamål.

##Hur man läser kommandoradsargument i Python

För att läsa kommandoradsargument i Python använder vi modulen "sys", som ger tillgång till en mängd olika funktioner för att arbeta med kommandoraden. För att läsa in argumenten används sys.argv, som är en lista som innehåller alla argument som matats in. Nedan följer ett exempel på hur man kan läsa in argument och skriva ut dem:

```Python
import sys

#Hämtar första argumentet (index 0 är alltid namnet på Python-filen)
argument1 = sys.argv[1]

#Hämtar andra argumentet
argument2 = sys.argv[2]

#Skriv ut argumenten till terminalen
print("Argument 1:", argument1)
print("Argument 2:", argument2)
```
Om du exempelvis kör detta program med kommandot "python minprogram.py hej världen" så kommer terminalen att visa:

```
Argument 1: hej
Argument 2: världen
```

För att göra det enklare för användaren att förstå vilka argument som förväntas, kan du också lägga till en beskrivning för varje argument när du kör programmet. Detta görs genom att skriva en sträng i sys.argv-funktionen:

```Python
sys.argv(["Beskrivning av argument 1", "Beskrivning av argument 2"])
```

## Fördjupning av läsning av kommandoradsargument 

En mer avancerad teknik för att läsa kommandoradsargument är att använda modulen "argparse". Denna modul ger en möjlighet att skapa mer robusta och flexibla program med stöd för flaggor och argument med olika typer av värden. Nedan följer ett exempel på hur du kan använda argparse för att läsa ett argument av typen "str" och en "flagga" (booleskt värde):

```Python
import argparse

#Skapar ett objekt av typen ArgumentParser
parser = argparse.ArgumentParser()

#Lägger till ett argument med typen str
parser.add_argument("namn", help="Ditt namn")

#Lägger till en flagga med typen bool
parser.add_argument("-e", "--ensamhem", help="Om du bor i ensamhushåll", action="store_true")

#Parserar argumenten som skickats in
args = parser.parse_args()

# Skriver ut varje argument
print("Hej", args.namn)

# Om ensamhushåll flaggan är satt
if args.ensamhem:
  print("Du bor i ensamhushåll")
```

Om du exempelvis kör detta program med kommandot "python minprogram.py Johanna -e" så kommer terminalen att visa:

```
Hej Johanna
Du bor i ensamhushåll
```

Det finns många fler möjligheter med att använda argparse, vilket gör det till en bra modul att utforska för att skapa mer avancerade program.

## Se också 
- [Python's documentation on sys module](https://docs.python.org/3/library/sys.html)
- [Python's documentation on argparse module](https://docs.python.org/3/library/argparse.html)
- [Real Python's tutorial on command line arguments](https://realpython.com/command-line-arguments-python/)