---
title:                "Python: Konvertere en streng til små bokstaver"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Å konvertere en streng til små bokstaver er en vanlig oppgave i Python-programmering. Dette kan være nyttig når du ønsker å sammenligne to strenger uten å ta hensyn til store og små bokstaver, eller for å sikre ensartethet i dataene dine. Ved å lære hvordan du gjør dette, kan du utvide kunnskapen din om grunnleggende strengmanipulasjon og forbedre programmene dine.

## Hvordan å gjøre det

For å konvertere en streng til små bokstaver i Python, kan du bruke metoden `lower()`. Denne metoden konverterer alle store bokstaver i en streng til små bokstaver. La oss se på et eksempel:

```python
streng = "HEI PÅ DEG"
print(streng.lower())
```

Dette vil gi oss følgende output:

```python
hei på deg
```

Som du kan se, har alle de store bokstavene i strengen blitt omgjort til små. Du kan også bruke metoden `casefold()`, som i tillegg til å konvertere store bokstaver, også håndterer spesielle tegn og diakritiske tegn. Denne metoden er nyttig når du ønsker å sammenligne to strenger, da den også tar hensyn til forskjeller i tegnsetting og diakritiske tegn.

```python
streng1 = "Åge"
streng2 = "Age"

print(streng1.lower() == streng2.lower())
print(streng1.casefold() == streng2.casefold())
```

Dette vil gi oss følgende output:

```python
False
True
```

Som du kan se, er den første sammenligningen ikke sann, da forskjellen i diakritiske tegn gjør en forskjell. Men med `casefold()` blir begge sammenligningene sanne.

## Dypdykk

Nå som du har lært grunnleggende om å konvertere en streng til små bokstaver, la oss se på noen ting du bør være klar over når du bruker disse metodene. Først og fremst, husk at konverteringen er ikke-destruktiv, noe som betyr at den ikke endrer den originale strengen, men heller returnerer en kopi av strengen med de nye endringene.

Du bør også være oppmerksom på at disse metodene bare fungerer for ASCII-tegn, og ikke for alle språk. For å kunne håndtere ikke-ASCII-tegn, må du bruke et annet bibliotek eller en annen metode. Videre, hvis du ønsker å konvertere en streng til store bokstaver, kan du bruke metoden `upper()` på samme måte som `lower()` ble brukt.

## Se også

- [Python String Methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Intro to Programming with Python](https://www.udacity.com/course/introduction-to-python--ud1110)
- [Official Python Documentation](https://docs.python.org/3/library/stdtypes.html#sttring-methods)