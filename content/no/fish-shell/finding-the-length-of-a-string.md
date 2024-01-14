---
title:    "Fish Shell: Å finne lengden på en streng"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Mange programmeringsoppgaver krever å finne lengden av en streng, enten det er for å validere brukerinndata eller manipulere tekst. I denne bloggposten vil vi utforske hvordan du kan bruke Fish Shell for å finne lengden av en streng.

## Hvordan

Det første vi trenger å gjøre er å definere en variabel som inneholder strengen vi ønsker å finne lengden av. Dette kan gjøres ved å skrive følgende i Fish Shell:

```Fish Shell
set string 'Hei, verden!'
```

Vi kan da bruke kommandoen `string length` til å finne lengden av strengen vår:

```Fish Shell
set length (string length $string)
echo $length
```

Dette vil gi oss følgende utskrift:

```
12
```

Vi kan også kombinere kommandoer for å finne lengden av en variabel på en mer kompakt måte:

```Fish Shell
echo (string length (string match -r -c "e" $string))
```

Dette vil gi ut lengden av strengen med alle forekomster av bokstaven "e" talt med.

## Dypdykk

Fish Shell har mange nyttige funksjoner for å manipulere og analysere tekst. En måte å finne ut mer om disse på, er ved å bruke kommandoen `string inspection`.

For å få en komplett oversikt over alle tilgjengelige funksjoner for å finne lengden av en streng, kan du bruke kommandoen `string help length`.

## Se også

- [Fish Shell dokumentasjon om lengde av strenger](https://fishshell.com/docs/current/cmds/string.html#length)
- [Flere eksempler på å finne lengden av en streng i Fish Shell](https://stackoverflow.com/questions/14140676/how-do-i-find-the-length-of-a-string-in-fish-shell)