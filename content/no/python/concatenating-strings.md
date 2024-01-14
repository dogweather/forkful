---
title:    "Python: Sammenslåing av strenger."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Å kombinere, eller "konkatenere", strenger er en vanlig operasjon i programmering, spesielt når du jobber med tekstbaserte applikasjoner. Det lar deg sette sammen flere strenger til en enkelt streng, noe som kan være nyttig for å lage komplekse setninger eller formatering av tekst.

## Hvordan

For å konkatenere strenger i Python, kan du bruke operatoren "+" eller metoden ".join()". La oss se på noen eksempler:

```Python 
# Bruker "+" operatoren
fornavn = "Lars"
etternavn = "Olsen"
navn = fornavn + " " + etternavn
print(navn)

# output: Lars Olsen
```

``` Python
# Bruker ".join()" metoden
farger = ["rød", "grønn", "blå"]
delimeter = " - "
farge_string = delimeter.join(farger)
print(farge_string)

# output: rød - grønn - blå
```

Det er viktig å merke seg at begge måtene kan bare konkatenere strenger. Hvis du vil kombinere en streng med et tall, må du først konvertere tallet til en streng ved hjelp av "str()" funksjonen.

## Dypdykk

Når du konkatenere strenger, oppretter du faktisk en helt ny streng, selv om du bruker eksisterende strenger. Dette skyldes at strenger i Python er uforanderlige, noe som betyr at de ikke kan endres direkte. Når du konkatenere strenger, opprettes det en kopi av de eksisterende strengene og kombineres for å danne en ny streng.

Et annet viktig konsept å huske på er at rekkefølgen på strengene du konkatenere betyr noe. Hvis du bytter rundt på rekkefølgen på strengene, vil du få en annen resultatstreng. Dette er spesielt viktig hvis du har et tomrom eller spesialtegn som en del av strengen.

## Se Også

- [Dokumentasjon for strenger i Python](https://docs.python.org/3/library/stdtypes.html#text-sequence-type-str)
- [Tutorial om strenger i Python](https://realpython.com/python-strings/)
- [Mer om tekstbehandling i Python](https://www.tutorialspoint.com/python/python_strings.htm)