---
title:                "Python: Stor bokstav i en streng"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg om å kapitalisere en streng i Python? Vel, det er ofte nødvendig å gjøre om strenger til store bokstaver for å matche bestemte formater eller for å gjøre dataenkoding enklere å forstå. Uansett, å kunne kapitalisere en streng er et viktig verktøy i en Python-programmerers verktøykasse.

## Hvordan

Det er flere måter å kapitalisere en streng på i Python. En av de enkleste er ved hjelp av .upper() metoden:

```Python
streng = "hei alle sammen!"
print(streng.upper())
```

**Output:** HEI ALLE SAMMEN!

Som du kan se, konverteres alle små bokstaver til store bokstaver ved hjelp av .upper() metoden. Dette er nyttig hvis du for eksempel ønsker å printe en tittel eller en overskrift.

Du kan også bruke .capitalize() metoden for å kun kapitalisere den første bokstaven i strengen:

```Python
streng = "dette er en test"
print(streng.capitalize())
```

**Output:** Dette er en test

En annen måte å kapitalisere en streng på er ved hjelp av str.upper() funksjonen:

```Python
streng = "jeg heter Peter"
print(str.upper(streng))
```

**Output:** JEG HETER PETER

## Dypdykk

For å forstå hvordan disse metodene og funksjonene fungerer, må du ha kunnskap om strenger i Python. Strenger er en sekvens av tegn og kan manipuleres ved hjelp av forskjellige metoder og funksjoner. Når du bruker .upper() metoden, konverteres alle bokstaver i strengen til store bokstaver mens .capitalize() metoden kun kapitaliserer den første bokstaven i strengen.

Det er også viktig å nevne at .upper() og .capitalize() metodene ikke endrer den opprinnelige strengen, men heller returnerer en ny streng med den kapitaliserte versjonen. Derfor kan du lagre den kapitaliserte strengen i en variabel og fortsette å bruke den opprinnelige strengen som den er.

## Se også

Her er noen nyttige ressurser for å lære mer om kapitalisering av strenger i Python:

- [Offisiell dokumentasjon for strengmetoder i Python](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Enkel guide for å kapitalisere strenger i Python](https://www.digitalocean.com/community/tutorials/how-to-capitalize-strings-in-python-3)
- [Video tutorial om manipulering av strenger i Python](https://www.youtube.com/watch?v=rfscVS0vtbw)

Takk for at du leste denne blogginnlegget om hvordan du kan kapitalisere en streng i Python. Vi håper det var nyttig for deg og at du nå føler deg mer komfortabel med å arbeide med strenger i kodespråket vårt. Lykke til videre med programmering!