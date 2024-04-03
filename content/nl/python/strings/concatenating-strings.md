---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:26.148606-07:00
description: "Strings samenvoegen betekent ze aan elkaar plakken om een nieuwe string\
  \ te maken. Het is zoals Lego met strings. We doen dit om tekst op te bouwen; denk\u2026"
lastmod: '2024-03-13T22:44:50.368333-06:00'
model: gpt-4-0125-preview
summary: Strings samenvoegen betekent ze aan elkaar plakken om een nieuwe string te
  maken.
title: Samenvoegen van strings
weight: 3
---

## Hoe:
Laten we wat strings aan elkaar plakken.

```python
voornaam = "Charlie"
achternaam = "Brown"
volledige_naam = voornaam + " " + achternaam  # Klassieke samenvoeging met een spatie
print(volledige_naam)
```
Uitvoer: `Charlie Brown`

Gebruikmakend van `join()` voor een lijst met woorden:

```python
woorden = ["Hallo", "wereld!"]
zin = " ".join(woorden)
print(zin)
```
Uitvoer: `Hallo wereld!`

F-String (sinds Python 3.6):

```python
gebruiker = "snoopy"
actie = "vliegend"
log_bericht = f"{gebruiker} is {actie} zijn hondenhok"
print(log_bericht)
```
Uitvoer: `snoopy is vliegend zijn hondenhok`

## Diepere Duik
Samenvoegen is een fundamentele stringbewerking sinds het begin van het programmeren. Onthoud, Python behandelt strings als onveranderlijk, dus elke samenvoeging creëert een nieuwe string.

Ooit was de plus (`+`) alles wat we hadden. Niet efficiënt voor meerdere strings, omdat het kon leiden tot geheugenzwelling en trage prestaties. Hier komt de `join()` methode—vriendelijker voor het geheugen, vooral voor het samenvoegen van een reeks strings.

F-Strings, geïntroduceerd in Python 3.6, zijn een gamechanger. Ze zijn leesbaar en snel en staan toe dat expressies binnen string-literalen worden geëvalueerd—`f"{variabele}"`. Ze zijn de eerste keuze voor een moderne Pythonista, die functionaliteit en efficiëntie combineert.

## Zie Ook
- [Python String Methoden](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [PEP 498 -- Letterlijke String Interpolatie](https://www.python.org/dev/peps/pep-0498/)
- [Python String Formatteer Best Practices](https://realpython.com/python-f-strings/)
