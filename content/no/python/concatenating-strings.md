---
title:                "Python: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor
Sammenføyde strenger, eller å legge sammen to eller flere tekststrenger, er en viktig del av Python-programmering. Det lar deg kombinere ulike deler av tekst for å danne en komplett streng, som kan være nyttig i både skripting og applikasjonsutvikling.

## Hvordan gjøre det
Det er flere måter å legge sammen strenger på i Python. Den enkleste metoden er å bruke "+" operatøren mellom tekststrenger, som vist i eksempelet nedenfor:

```python
navn = "Lars"
tillegg = " er en programmerer."
print(navn + tillegg)
```
Dette vil gi følgende utskrift:

```python
Lars er en programmerer.
```
Du kan også bruke "-=" operatøren for å legge sammen en eksisterende streng med en ny streng, som vist i eksempelet nedenfor:

```python
tekst = "Python er et "
tekst -= "kraftig programmeringsspråk."
print(tekst)
```
Dette vil gi følgende utskrift:

```python
Python er et kraftig programmeringsspråk.
```
En annen metode for å sammenføye strenger er å bruke "%", som også er kjent som "string formatering". Dette lar deg sette inn variabler eller verdier i en streng på en strukturert måte. Se eksempelet nedenfor:

```python
navn = "Marie"
alder = 25
print("%s er %d år gammel." % (navn, alder))
```
Dette vil gi følgende utskrift:

```python
Marie er 25 år gammel.
```

## Dypt dykk 
I tillegg til de grunnleggende metodene beskrevet ovenfor, finnes det også flere funksjoner og metoder i Python for å sammenføye strenger. En av dem er "join()" metoden, som lar deg kombinere en liste av strenger til en enkelt streng. Se eksempelet nedenfor:

```python
tekst = ["Python", "er", "et", "kraftig", "programmeringsspråk."]
print(" ".join(tekst))
```
Dette vil gi følgende utskrift:

```python
Python er et kraftig programmeringsspråk.
```
En annen nyttig metode er "format()" som gir en mer strukturert måte å sette inn variabler eller verdier i en streng på. Se eksempelet nedenfor:

```python
navn = "Petter"
alder = 20
print("{} er {} år gammel.".format(navn, alder))
```
Dette vil gi følgende utskrift:

```python
Petter er 20 år gammel.
```

## Se også
- [Offisiell Python dokumentasjon om strenger](https://docs.python.org/3/library/stdtypes.html#str)
- [Enkel veiledning til strenger i Python](https://realpython.com/python-strings/)

Forhåpentligvis har denne artikkelen hjulpet deg med å forstå hvorfor og hvordan man legger sammen strenger i Python. Lykke til med å implementere dette i dine egne prosjekter!