---
title:    "Python: Slette tegn som matcher et mønster"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Hvorfor

Som Python-programmer vil du mest sannsynlig finne deg i situasjoner hvor du trenger å manipulere tekststrenger. En vanlig oppgave er å fjerne spesifikke tegn eller symboler fra en tekststreng, enten det er for å gjøre den mer lesbar eller for å behandle den videre. En måte å oppnå dette på er å bruke Python-funksjonen `re.sub()`, som lar deg fjerne tegn som matcher et gitt mønster. I denne artikkelen skal vi se på hvordan du kan bruke denne funksjonen og hva du kan oppnå med den.

# Hvordan

For å bruke `re.sub()`-funksjonen, må du først importere `re`-modulen. Deretter kan du bruke følgende syntaks:

```Python
ny_streng = re.sub(mønster, nytt_tegn, tekst)
```

La oss si at vi har en tekststreng som inneholder telefonnumre, men vi ønsker å fjerne alle bindestreker fra numrene. Vi kan da bruke følgende kode:

```Python
import re
tekst = "Mitt telefonnummer er 123-456-7890"
ny_tekst = re.sub("-", "", tekst)

print(ny_tekst)
```

Denne koden vil produsere følgende utskrift:

```
Mitt telefonnummer er 1234567890
```

Her erstatter vi bindestrekene med et tomt tegn, som sier til `re.sub()`-funksjonen at vi ønsker å fjerne alle bindestreker fra teksten.

Det er også mulig å bruke regulære uttrykk i mønsteret. Dette gir deg mer fleksibilitet når du skal fjerne mønstre fra en tekststreng. La oss si at vi nå ønsker å fjerne all tekst som ikke er tall fra teksten vår. Vi kan gjøre dette ved å bruke følgende kode:

```Python
import re
tekst = "Hei, mitt telefonnummer er 123-456-7890!"
ny_tekst = re.sub("[^0-9]", "", tekst)

print(ny_tekst)
```

Dette vil produsere følgende utskrift:

```
1234567890
```

Her bruker vi uttrykket `[^0-9]` i stedet for bare et tegn. Dette betyr at vi vil fjerne alt som ikke er tall fra teksten.

# Dypdykk

En ting å være oppmerksom på når du bruker `re.sub()`-funksjonen, er at den bruker regulære uttrykk i mønstrene. Dette innebærer at du må være klar over eventuelle spesielle tegn som kan bety noe annet i et regulært uttrykk enn i en vanlig tekststreng. For eksempel vil tegnet `.` i et regulært uttrykk matche hvilket som helst tegn, mens det i en tekststreng kun vil matche selve punktumet.

Det er også viktig å merke seg at `re.sub()`-funksjonen kun vil fjerne én forekomst av et mønster om gangen. Hvis du vil fjerne alle forekomster, må du bruke en løkke eller angi parameteren `count` til `0` eller større.

# Se også

- [Python re-sub dokumentasjon](https://docs.python.org/3/library/re.html#re.sub)
- [En interaktiv tutorial for regulære uttrykk i Python](https://www.datacamp.com/community/tutorials/python-regular-expression-tutorial) 
- [En oversikt over vanlige regulære uttrykk](https://www.regular-expressions.info/cheatsheet.html)