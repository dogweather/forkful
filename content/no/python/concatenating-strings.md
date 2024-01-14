---
title:    "Python: Sammenføying av strenger"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger i programmering, vil du trenge å kombinere to eller flere strenger for å lage en ny streng. Dette kalles "konkatinering" og det kan være veldig nyttig for å manipulere og lage mer komplekse data.

## Hvordan

Her er noen eksempler på hvordan man kan konkatinere strenger i Python:

```python
# Bruke "+" operator:
fornavn = "Kari"
etternavn = "Nordmann"
navn = fornavn + etternavn
print(navn)
```
Dette vil produsere outputen "KariNordmann".

```python
# Bruke "format" metoden:
tall = 42
navn = "Svar på alt er {}.".format(tall)
print(navn)
```
Dette vil produsere outputen "Svar på alt er 42.".

```python
# Bruke "join" metoden:
list = ["Eple", "Banan", "Jordbær"]
frukt = ", ".join(list)
print(frukt)
```
Dette vil produsere outputen "Eple, Banan, Jordbær".

## Deep Dive

I Python, er strenger immutable, noe som betyr at de ikke kan forandres etter at de er opprettet. Derfor, når man konkatinerer strenger, lager man en helt ny streng i stedet for å endre den eksisterende. Dette kan være viktig å huske på når man jobber med store strengoperasjoner.

En annen ting å være oppmerksom på er rekkefølgen på strengene man konkatinerer. For eksempel, vil "he" + "ll" + "o" produsere en annen streng enn "ll" + "o" + "he".

## Se også

Her er noen andre nyttige ressurser for å lære mer om å konkatinere strenger i Python:

- [Python String Concatenation and Formatting](https://realpython.com/python-string-concatenation/)
- [Learn Python the Hard Way: Exercise 6 - Strings and Text](https://learnpythonthehardway.org/python3/ex6.html)
- [Python String join() Method](https://www.w3schools.com/python/ref_string_join.asp)