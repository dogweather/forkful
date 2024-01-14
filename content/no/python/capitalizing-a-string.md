---
title:    "Python: Å FORVELTE EN STRENG"
keywords: ["Python"]
---

{{< edit_this_page >}}

# Hvorfor bruke store bokstaver i en streng?

Å gjøre en streng til store bokstaver kan være nyttig for å gjøre den mer lesbar eller for å matche eksisterende data. For eksempel kan det være nyttig å gjøre en brukers input til store bokstaver for å unngå problemer med sammenligning senere.

## Slik gjør du det i Python

```Python
# Definerer en variabel med en liten streng
streng = "dette er en liten streng"

# Gjør strengen til store bokstaver og lagrer den som en ny variabel
streng_med_store_bokstaver = streng.upper()

# Skriver ut resultatet
print(streng_med_store_bokstaver)

# Output: DETTE ER EN LITEN STRENG
```

## Dykk dypere ned i store bokstaver

Når man bruker ```upper()``` metoden i Python, vil den kun gjøre de små bokstavene i den originale strengen om til store. Andre tegn som tall eller spesialtegn forblir uendret. Du kan også bruke ```capitalize()``` for å gjøre første bokstav i strengen til stor bokstav og resten til små bokstaver.

Det finnes også andre måter å gjøre en streng til store bokstaver på, som å bruke ```str.title()``` for å gjøre hver første bokstav i hvert ord til stor bokstav. Det er viktig å merke seg at disse metodene ikke endrer den originale strengen, men heller returnerer en kopi med de ønskede endringene.

## Se også

- [Python dokumentasjon for strengmetoder](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Enkel veiledning til strings i Python](https://realpython.com/python-strings/)
- [En video om ulike strengmanipulasjoner i Python](https://www.youtube.com/watch?v=k9TUPpGqYTo)