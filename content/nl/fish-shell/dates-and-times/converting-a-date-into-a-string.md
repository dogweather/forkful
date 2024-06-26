---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:13.315803-07:00
description: 'Hoe: Fish shell houdt het eenvoudig. Laten we de huidige datum formatteren.'
lastmod: '2024-03-13T22:44:51.258735-06:00'
model: gpt-4-0125-preview
summary: Fish shell houdt het eenvoudig.
title: Een datum converteren naar een string
weight: 28
---

## Hoe:
Fish shell houdt het eenvoudig. Laten we de huidige datum formatteren:

```fish
set formatted_date (date "+%Y-%m-%d")
echo $formatted_date
```

Voorbeelduitvoer:
```
2023-04-11
```

Wil je iets specifiekers, zoals de dag van de week?

```fish
set day_of_week (date "+%A")
echo $day_of_week
```

Voorbeelduitvoer:
```
Dinsdag
```

Hoe zit het met het toevoegen van de tijd? Hier is de datum en tijd in een 24-uurs formaat:

```fish
set date_and_time (date "+%Y-%m-%d %H:%M:%S")
echo $date_and_time
```

Voorbeelduitvoer:
```
2023-04-11 21:30:47
```

## Diepere Duik
In het verleden hebben Unix-achtige systemen zoals Linux het `date` commando aangenomen, dat in de loop van de tijd is geëvolueerd en nog steeds veel voorkomt in shells zoals bash en zsh. Fish shell erft dit, maar moedigt een meer leesbare, vlagloze syntaxis aan voor het instellen van variabelen.

Er zijn alternatieven, zoals de `strftime` functie in veel programmeertalen. Fish ondersteunt dit niet van nature, maar `date` in UNIX is veelzijdig genoeg om aan de meeste behoeften te voldoen.

Bij het omzetten van een datum naar een tekenreeks volgen de formaatspecificatoren, zoals `%Y` voor het jaar of `%A` voor de weekdag, de POSIX-standaard. Het `date` commando gebruikt deze specificatoren om specifieke delen van de datum te extraheren en te formatteren.

Het is belangrijk om op te merken dat, omdat data en tijden zo afhankelijk zijn van de lokale tijd en tijdzone, de geproduceerde tekenreeksen kunnen variëren tenzij gespecificeerd. Je kunt de tijdzone instellen voor het aanroepen van `date`:

```fish
set TZ 'America/New_York'
set date_with_timezone (date "+%Y-%m-%d %H:%M:%S %Z")
echo $date_with_timezone
```

Dit zorgt ervoor dat je de locatiegevoeligheid van je gegevens hebt overwogen - een detail dat niet over het hoofd mag worden gezien in een geglobaliseerde wereld.

## Zie Ook
- De `man` pagina voor `date` ([online handleiding](https://linux.die.net/man/1/date)) geeft je de volledige uitleg over formaatspecificatoren.
- Voor een bredere context, lees over [POSIX-standaarden](https://nl.wikipedia.org/wiki/POSIX).
- Bekijk de officiële documentatie van Fish shell over [variabelen](https://fishshell.com/docs/current/language.html#variables) om de `set` opdracht beter te begrijpen.
