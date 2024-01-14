---
title:    "Python: Sletting av tegn som matcher et mønster"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger i programmering må vi manipulere tekst ved å slette visse tegn som matcher et bestemt mønster. Dette kan være nyttig for å rense og organisere data, eller for å utføre spesielle operasjoner på tekststrenger.

## Hvordan

For å slette tegn som matcher et mønster i Python, kan vi bruke `re.sub()`-funksjonen fra `re`-biblioteket. La oss si at vi ønsker å fjerne alle tall fra en tekststreng. Vi kan bruke følgende kode:

```Python
import re

tekststreng = "Dette 123 er en tekst 456 med tall"
renset_tekst = re.sub(r"\d+", "", tekststreng)

print(renset_tekst) # Output: Dette er en tekst med tall
```

La oss bryte ned koden: Først importerer vi `re`-biblioteket for å bruke `re.sub()`-funksjonen. Deretter definerer vi en variabel `tekststreng` som inneholder teksten vi ønsker å manipulere. Videre bruker vi `re.sub()`-funksjonen med et regulært uttrykk (`r"\d+"`) som matcher alle tall i strengen, og erstatter dem med en tom streng `""`. Til slutt skriver vi ut den rensete teksten.

Vi kan også gjøre mer avanserte manipulasjoner, som å fjerne alle spesialtegn fra en tekststreng:

```Python
import re

tekststreng = "Dette er en tekst !§\"@#$%&/()= med spesialtegn"
renset_tekst = re.sub(r"[^\w\s]+", "", tekststreng)

print(renset_tekst) # Output: Dette er en tekst med spesialtegn
```

I dette tilfellet bruker vi et negativt sett `[^\w\s]+` i det regulære uttrykket for å matche alle tegn som ikke er bokstaver eller mellomrom, og erstatter dem med en tom streng.

## Dypdykk

Det finnes også andre nyttige funksjoner i `re`-biblioteket for å slette karakterer som matcher et mønster, som for eksempel `re.subn()` som returnerer antall erstattede forekomster, eller `re.split()` som deler en tekststreng ved mønstermatchen.

Det er viktig å være nøye med regulære uttrykk når man jobber med `re`-biblioteket, da små feil kan føre til uønskede resultater eller runtime-feil. Det kan være lurt å teste ut regulære uttrykk i en online verktøykasse som regex101.com før man implementerer dem i koden.

## Se også

- [Offisiell dokumentasjon for `re`-biblioteket](https://docs.python.org/3/library/re.html)
- [Python Regex Tutorial](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial) (engelsk)
- [Regex Tester](https://regex101.com/) (engelsk)