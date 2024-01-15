---
title:                "Sammenføyning av strenger"
html_title:           "Python: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Noe av det mest grunnleggende og viktige i programmering er å kunne håndtere tekst eller strenger. En måte å gjøre dette på er å kombinere, eller "konkatenerere", flere strenger til en enkel streng. Dette kan være nyttig for å lage dynamisk tekst eller for å bygge kompliserte kommandoer.

## Slik gjør du det

For å konkatenerere strenger i Python, kan du bruke operatøren "+" eller metoden "join()". Her er et eksempel:

```python
# Eksempel på konkatenering ved å bruke "+"
fornavn = "Lars"
etternavn = "Johansen"
fullt_navn = fornavn + " " + etternavn
print(fullt_navn) # Output: Lars Johansen

# Eksempel på konkatenering ved å bruke "join()"
hobbyer = ["fotball", "musikk", "matlaging"]
tekst = "Mine hobbyer er: " + ", ".join(hobbyer)
print(tekst) # Output: Mine hobbyer er: fotball, musikk, matlaging
```

Merk at når du bruker operatøren "+", må alle operandene være strenger. Med metoden "join()", kan du konkatenerere både strenger og lister.

## Dypdykk

I tillegg til å bruke "+" og "join()", finnes det andre måter å konkatenerere strenger på i Python. For eksempel kan du bruke formateringsstrenger, som gir mer kontroll over hvordan strengene kombineres. Her er et eksempel:

```python
# Eksempel på formateringsstrenger for konkatenering
fornavn = "Petter"
etternavn = "Olsen"
alder = 30
tekst = "Jeg heter {0} {1} og jeg er {2} år gammel".format(fornavn, etternavn, alder)
print(tekst) # Output: Jeg heter Petter Olsen og jeg er 30 år gammel
```

Formateringsstrenger gir også mulighet for å formatere verdier, som å legge til desimaler eller spesifisere lengden på en streng.

## Se også

- [Python string concatenation tutorial](https://www.programiz.com/python-programming/string-concatenation)
- [Python string formatting](https://docs.python.org/3/library/string.html#format-string-syntax)
- [Python string methods](https://www.w3schools.com/python/python_ref_string.asp)