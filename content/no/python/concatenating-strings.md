---
title:                "Sammenslåing av strenger"
html_title:           "Python: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konkatenasjon av strenger er en metode som tillater programmerere å kombinere flere strenger til en enkelt streng. Dette er spesielt nyttig når man ønsker å lage en dynamisk tekst som inneholder variabler eller brukerinput. 

## Hvordan:
Her er et eksempel på hvordan du konkatenere strenger i Python:

```python
name = "Jonas"
age = 28
hobby = "fotografering"

# Kombinerer variabler i en setning med pluss operatøren
sentence = "Hei, jeg heter " + name + " og jeg er " + str(age) + " år gammel. Jeg elsker " + hobby + "!"

print(sentence)
```

Output:
```python
Hei, jeg heter Jonas og jeg er 28 år gammel. Jeg elsker fotografering!
```

Du kan også bruke `.format()` metoden for å konkatenere strenger:

```python
fruit = "eple"

# Formatere strengen for å sette inn variabelen
sentence = "Jeg elsker {}.".format(fruit)

print(sentence)
```

Output:
```python
Jeg elsker eple.
```

## Dypdykk:
Metoden for å konkatenere strenger stammer fra matematisk teori og har blitt en vanlig praksis i programmeringsspråk. En alternativ måte å kombinere strenger på er ved hjelp av `.join()` metoden, men konkatenasjon kan ofte være enklere å bruke, spesielt for nybegynnere.

Når man konkatenere strenger, er det viktig å huske på datatyper. Hvis en variabel er tall, må du bruke `str()` funksjonen for å konvertere til en streng før du kan kombinere den med andre strenger. 

## Se også:
- [Python Offisiell Dokumentasjon](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [W3Schools: String Concatenation](https://www.w3schools.com/python/gloss_python_string_concatenation.asp)