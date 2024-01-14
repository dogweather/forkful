---
title:                "Python: Sammenslåing av strenger"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

I programmering er det ofte nødvendig å kombinere flere tekststrenger for å lage en ny tekst. Denne prosessen kalles "concatenation" på engelsk, og i Python brukes "+" operatøren for å gjøre dette. Ved å lære hvordan man konkatenerer strenger, kan du utvide dine muligheter som programmerer og lage mer avanserte og dynamiske programmer.

## Hvordan

```python
text1 = "Hei"
text2 = "verden!"
text3 = text1 + text2
print(text3)
```

Output:
```python
Hei verden!
```

I dette eksempelet har vi to variabler, "text1" og "text2", som inneholder hver sin tekststreng. Ved å bruke "+", blir disse to strengene koblet sammen og lagret i den tredje variabelen, "text3". Deretter blir den concatenerte strengen "Hei verden!" printet ut. 

En annen måte å concatenate strenger på er å bruke % tegnet og et formatert tekststreng:

```python
name = "Maria"
age = 25
text = "Hei, mitt navn er %s og jeg er %d år gammel." % (name, age)
print(text)
```

Output:
```python
Hei, mitt navn er Maria og jeg er 25 år gammel.
```

Her blir "%s" og "%d" brukt til å plassere variablene "name" og "age" inn i teksten. Dette er spesielt nyttig når du ønsker å inkludere variabler i en lengre tekststreng.

## Deep Dive

I tillegg til å bruke "+" og "%"-operatørene, kan du også bruke "join" funksjonen for å concatenate strenger. Dette er spesielt nyttig når du ønsker å kombinere en liste av strenger. For å bruke "join", må du først spesifisere hvordan du ønsker å separere strengene, og deretter spesifisere listen du ønsker å concatenate.

```python
names = ["Per", "Kari", "Ole"]
hello_string = "Hei, mitt navn er"
joined_string = hello_string.join(names)
print(joined_string)
```

Output:
```python
Hei, mitt navn er PerHei, mitt navn er KariHei, mitt navn er Ole
```

Det er også viktig å huske at når du konkatenerer strenger, må du sørge for at alle strengene er av samme datatype. Ellers vil du få en feilmelding.

## Se også

- [Offisiell Python dokumentasjon for concatenating strenger](https://docs.python.org/3.7/library/string.html#formatstrings)
- [W3Schools tutorial om concatenating strenger i Python](https://www.w3schools.com/python/python_strings_concatenate.asp)
- [RealPython tutorial om concatenating strenger i Python](https://realpython.com/python-string-split-concatenate-join/)