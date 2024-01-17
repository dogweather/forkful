---
title:                "Utskrift av feilsøkingsutdata"
html_title:           "Python: Utskrift av feilsøkingsutdata"
simple_title:         "Utskrift av feilsøkingsutdata"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Printing av feilsøkningsutdata er en vanlig praksis blant programmerere for å hjelpe med å identifisere og løse problemer i koden sin. Det innebærer å skrive ut variabler, meldinger og annen relevant informasjon under kjøringen av programmet ditt.

## Slik gjør du:

```
Python print("Hei, verden!") 
```

Dette enkle eksempelet viser hvordan du bruker `print` -funksjonen til å skrive ut en melding i Python. Du kan også skrive ut variabler og til og med matematiske uttrykk ved å legge dem til i parentes etter `print` -funksjonen.

```
Python tall = 5 print("Tallet er:", tall) #Output: Tallet er: 5 print("Summen av 2+3 er:", tall+2) #Output: Summen av 2+3 er: 7 
```

## Dypdykk:

Printing av feilsøkningsutdata har vært en viktig metode for å finne feil i koden siden de tidlige dagene av programmering. Alternativet til å bruke `print` -funksjonen er å bruke en debugger, som er et spesialisert verktøy for å spore og fikse feil. Selv om det kan være mer effektivt i visse tilfeller, kan det være vanskeligere for nybegynnere å bruke.

Under panseret bruker `print` -funksjonen `sys.stdout` -objektet for å skrive ut data til standard utdatastrømmen. Dette er vanligvis skjermen, men det kan også omadresseres til en fil. Du kan også bruke `sys.stderr` for å skrive ut feilmeldinger.

## Se også:

- [Python `print` -dokumentasjon](https://docs.python.org/3/library/functions.html#print)
- [Debugging for nybegynnere](https://wiki.python.org/moin/DebuggingWithPython)
- [Hvordan bruke en debugger i Python](https://realpython.com/python-debugging-pdb/)